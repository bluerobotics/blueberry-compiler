use blueberry_ast::{
    Annotation, AnnotationParam, Commented, ConstDef, ConstValue, Definition, EnumDef, MessageDef,
    StructDef, Type, TypeDef,
};
use blueberry_codegen_core::{CodegenError, DEFAULT_MODULE_KEY, GeneratedFile, TypeRegistry};
use genco::lang::c::Tokens;
use genco::quote;

const OUTPUT_PATH: &str = "cpp/messages.hpp";

pub fn generate(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let generator = CppGenerator::new(definitions);
    let contents = generator.render(definitions)?;

    Ok(vec![GeneratedFile {
        path: OUTPUT_PATH.to_string(),
        contents,
    }])
}

struct CppGenerator {
    registry: TypeRegistry,
}

impl CppGenerator {
    fn new(definitions: &[Definition]) -> Self {
        Self {
            registry: TypeRegistry::new(definitions),
        }
    }

    fn render(&self, definitions: &[Definition]) -> Result<String, CodegenError> {
        let defs = self.emit_definitions(definitions, &mut Vec::new(), DEFAULT_MODULE_KEY)?;
        let helpers = helpers_tokens();
        let tokens: Tokens = quote! {
            #pragma once

            #include <array>
            #include <cstddef>
            #include <cstdint>
            #include <cstring>
            #include <span>
            #include <stdexcept>
            #include <string>
            #include <string_view>
            #include <type_traits>
            #include <utility>
            #include <vector>

            namespace blueberry_generated {

            $helpers

            $(for def in defs =>
                $def

            )

            } // namespace blueberry_generated
        };

        Ok(tokens.to_file_string().expect("render cpp output"))
    }

    fn emit_definitions(
        &self,
        defs: &[Definition],
        scope: &mut Vec<String>,
        module_key: u16,
    ) -> Result<Vec<Tokens>, CodegenError> {
        let mut out = Vec::new();
        let mut message_keys = MessageKeyGen::default();

        for def in defs {
            match def {
                Definition::ModuleDef(module) => {
                    let module_key =
                        annotation_u16(&module.annotations, "module_key").unwrap_or(module_key);
                    scope.push(module.node.name.clone());
                    let nested =
                        self.emit_definitions(&module.node.definitions, scope, module_key)?;
                    scope.pop();

                    let name = &module.node.name;
                    out.push(quote! {
                        namespace $name {
                        $(for item in nested =>
                            $item

                        )
                        } // namespace $name
                    });
                }
                Definition::EnumDef(enum_def) => {
                    out.push(self.emit_enum(enum_def, scope));
                    out.push(quote! { $['\n'] });
                }
                Definition::StructDef(struct_def) => {
                    out.push(self.emit_struct(struct_def, scope));
                    out.push(quote! { $['\n'] });
                }
                Definition::MessageDef(message_def) => {
                    let message_key = annotation_u16(&message_def.annotations, "message_key")
                        .unwrap_or_else(|| message_keys.next());
                    out.push(self.emit_message(message_def, scope, module_key, message_key)?);
                    out.push(quote! { $['\n'] });
                }
                Definition::ConstDef(const_def) => {
                    out.push(self.emit_const(const_def, scope));
                }
                Definition::TypeDef(typedef_def) => {
                    out.push(self.emit_typedef(typedef_def, scope));
                }
                Definition::ImportDef(_) => {}
            }
        }

        Ok(out)
    }

    fn emit_enum(&self, enum_def: &Commented<EnumDef>, scope: &[String]) -> Tokens {
        let scoped = &enum_def.node.name;
        let base = enum_def
            .node
            .base_type
            .clone()
            .unwrap_or(Type::UnsignedLong);
        let base_ty = self.cpp_type(&base, scope);
        let members: Vec<Tokens> = enum_def
            .node
            .enumerators
            .iter()
            .map(|member| {
                if let Some(ConstValue::Integer(value)) = &member.value {
                    quote!( $(member.name.clone()) = $(value.value) )
                } else {
                    quote!( $(member.name.clone()) )
                }
            })
            .collect();

        quote! {
            enum class $scoped : $base_ty {
                $(for m in members join (, ) => $m)
            };
        }
    }

    fn emit_struct(&self, struct_def: &Commented<StructDef>, scope: &[String]) -> Tokens {
        let mut path = scope.to_vec();
        path.push(struct_def.node.name.clone());
        let members = self.registry.collect_struct_members(&path);
        let fields: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let ty = self.cpp_type(&member.ty, scope);
                let name = &member.name;
                quote!( $ty $name{}; )
            })
            .collect();
        let serialize_fields: Vec<Tokens> = members
            .iter()
            .map(|member| self.serialize_value(&member.name, &member.ty, scope, quote!(writer)))
            .collect();
        let deserialize_fields: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let var_name = format!("value_{}", member.name);
                let read_expr = self.deserialize_value(&member.ty, scope, quote!(reader));
                quote! {
                    auto $var_name = $read_expr;
                }
            })
            .collect();
        let assignments: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let var_name = format!("value_{}", member.name);
                let field = &member.name;
                quote!( result.$field = $var_name; )
            })
            .collect();

        let name = &struct_def.node.name;
        quote! {
            struct $name {
                $(
                    for field in &fields join ($['\r']) => $field
                )

                [[nodiscard]] std::vector<std::uint8_t> to_payload() const {
                    CdrWriter writer;
                    serialize(writer);
                    return writer.buffer;
                }

                void serialize(CdrWriter &writer) const {
                    $(for s in &serialize_fields => $s$['\r'])
                }

                [[nodiscard]] static $name from_payload(std::span<const std::uint8_t> payload) {
                    CdrReader reader(payload);
                    return read(reader);
                }

                [[nodiscard]] static $name read(CdrReader &reader) {
                    $(for d in &deserialize_fields => $d$['\r'])
                    $name result{};
                    $(for a in &assignments => $a$['\r'])
                    return result;
                }
            };
        }
    }

    fn emit_message(
        &self,
        message_def: &Commented<MessageDef>,
        scope: &[String],
        module_key: u16,
        message_key: u16,
    ) -> Result<Tokens, CodegenError> {
        let topic = annotation_string(&message_def.annotations, "topic").ok_or(
            CodegenError::MissingTopic {
                message: scoped_name(scope, &message_def.node.name),
            },
        )?;
        let mut path = scope.to_vec();
        path.push(message_def.node.name.clone());
        let members = self.registry.collect_message_members(&path);
        let fields: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let ty = self.cpp_type(&member.ty, scope);
                let name = &member.name;
                quote!( $ty $name{}; )
            })
            .collect();
        let serialize_fields: Vec<Tokens> = members
            .iter()
            .map(|member| self.serialize_value(&member.name, &member.ty, scope, quote!(writer)))
            .collect();
        let deserialize_fields: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let var_name = format!("value_{}", member.name);
                let read_expr = self.deserialize_value(&member.ty, scope, quote!(reader));
                quote! {
                    auto $var_name = $read_expr;
                }
            })
            .collect();
        let assignments: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let var_name = format!("value_{}", member.name);
                let field = &member.name;
                quote!( result.$field = $var_name; )
            })
            .collect();

        let name = &message_def.node.name;
        let mut schema = scope.join(".");
        if !schema.is_empty() {
            schema.push('.');
        }
        schema.push_str(name);
        let topic_literal = quoted_string(&topic);
        let schema_literal = quoted_string(&schema);

        Ok(quote! {
            struct $name {
                static constexpr std::uint16_t kModuleKey = $module_key;
                static constexpr std::uint16_t kMessageKey = $message_key;
                static constexpr std::string_view kTopicTemplate = $topic_literal;
                static constexpr std::string_view kSchema = $schema_literal;

                $(
                    for field in &fields =>
                        $field
                )

                [[nodiscard]] std::vector<std::uint8_t> to_payload() const {
                    CdrWriter writer;
                    serialize(writer);
                    return writer.buffer;
                }

                void serialize(CdrWriter &writer) const {
                    $(for s in &serialize_fields => $s$['\r'])
                }

                [[nodiscard]] static $name from_payload(std::span<const std::uint8_t> payload) {
                    CdrReader reader(payload);
                    return read(reader);
                }

                [[nodiscard]] static $name read(CdrReader &reader) {
                    $(for d in &deserialize_fields => $d$['\r'])

                    $name result{};
                    $(for a in &assignments join ($['\r']) => $a)
                    return result;
                }

                [[nodiscard]] static std::string topic() {
                    return format_topic(std::string(kTopicTemplate));
                }
            };
        })
    }

    fn emit_const(&self, const_def: &Commented<ConstDef>, scope: &[String]) -> Tokens {
        let name = &const_def.node.name;
        let ty = self.cpp_type(&const_def.node.const_type, scope);
        let value = const_literal(&const_def.node.value);
        quote!( inline constexpr $ty $name = $value; )
    }

    fn emit_typedef(&self, typedef_def: &Commented<TypeDef>, scope: &[String]) -> Tokens {
        let name = &typedef_def.node.name;
        let base = self.cpp_type(&typedef_def.node.base_type, scope);
        quote!( using $name = $base; )
    }

    fn cpp_type(&self, ty: &Type, scope: &[String]) -> Tokens {
        match self.registry.resolve_type(ty, scope) {
            Type::Float => quote!(float),
            Type::Double => quote!(double),
            Type::LongDouble => quote!(long double),
            Type::Long => quote!(std::int32_t),
            Type::UnsignedLong => quote!(std::uint32_t),
            Type::LongLong => quote!(std::int64_t),
            Type::UnsignedLongLong => quote!(std::uint64_t),
            Type::Short => quote!(std::int16_t),
            Type::UnsignedShort => quote!(std::uint16_t),
            Type::Octet => quote!(std::uint8_t),
            Type::Boolean => quote!(bool),
            Type::Char => quote!(char),
            Type::String { .. } => quote!(std::string),
            Type::Sequence { element_type, .. } => {
                let inner = self.cpp_type(&element_type, scope);
                quote!(std::vector<$inner>)
            }
            Type::Array {
                element_type,
                dimensions,
            } => {
                let mut elem = *element_type;
                for _ in dimensions {
                    elem = Type::Sequence {
                        element_type: Box::new(elem),
                        size: None,
                    };
                }
                self.cpp_type(&elem, scope)
            }
            Type::ScopedName(path) => {
                let scoped = absolute_path(&path);
                quote!( $scoped )
            }
            Type::WString | Type::WChar => quote!(std::u16string),
        }
    }

    fn serialize_value(&self, name: &str, ty: &Type, scope: &[String], writer: Tokens) -> Tokens {
        match self.registry.resolve_type(ty, scope) {
            Type::Boolean => quote!( $writer.write_bool($name); ),
            Type::Char => quote!( $writer.write_char($name); ),
            Type::Octet => quote!( $writer.write_u8($name); ),
            Type::Short => quote!( $writer.write_i16($name); ),
            Type::UnsignedShort => quote!( $writer.write_u16($name); ),
            Type::Long => quote!( $writer.write_i32($name); ),
            Type::UnsignedLong => quote!( $writer.write_u32($name); ),
            Type::LongLong => quote!( $writer.write_i64($name); ),
            Type::UnsignedLongLong => quote!( $writer.write_u64($name); ),
            Type::Float => quote!( $writer.write_f32($name); ),
            Type::Double => quote!( $writer.write_f64($name); ),
            Type::LongDouble => quote!( $writer.write_f64(static_cast<double>($name)); ),
            Type::String { .. } => quote!( $writer.write_string($name); ),
            Type::Sequence { element_type, .. } => {
                let inner = self.serialize_value("item", &element_type, scope, quote!(writer));
                quote! {
                    $writer.write_sequence($name, [](CdrWriter &writer, const auto &item) {
                        $inner
                    });
                }
            }
            Type::Array {
                element_type,
                dimensions,
            } => {
                let mut elem = *element_type;
                for _ in dimensions {
                    elem = Type::Sequence {
                        element_type: Box::new(elem),
                        size: None,
                    };
                }
                self.serialize_value(name, &elem, scope, writer)
            }
            Type::ScopedName(path) => {
                if let Some(base) = self.registry.enum_base(&path) {
                    let base_ty = self.cpp_type(base, scope);
                    let write_fn = match base {
                        Type::UnsignedShort => quote!(write_u16),
                        Type::Short => quote!(write_i16),
                        Type::UnsignedLong => quote!(write_u32),
                        Type::Long => quote!(write_i32),
                        Type::UnsignedLongLong => quote!(write_u64),
                        Type::LongLong => quote!(write_i64),
                        Type::Octet => quote!(write_u8),
                        Type::Boolean => quote!(write_bool),
                        Type::Char => quote!(write_char),
                        Type::Float => quote!(write_f32),
                        Type::Double => quote!(write_f64),
                        _ => quote!(write_u32),
                    };
                    quote! {
                        $writer.$write_fn(static_cast<$base_ty>($name));
                    }
                } else {
                    quote!( $name.serialize($writer); )
                }
            }
            Type::WString | Type::WChar => quote!( /* unsupported wide string */ ),
        }
    }

    fn deserialize_value(&self, ty: &Type, scope: &[String], reader: Tokens) -> Tokens {
        match self.registry.resolve_type(ty, scope) {
            Type::Boolean => quote!( $reader.read_bool() ),
            Type::Char => quote!( $reader.read_char() ),
            Type::Octet => quote!( $reader.read_u8() ),
            Type::Short => quote!( $reader.read_i16() ),
            Type::UnsignedShort => quote!( $reader.read_u16() ),
            Type::Long => quote!( $reader.read_i32() ),
            Type::UnsignedLong => quote!( $reader.read_u32() ),
            Type::LongLong => quote!( $reader.read_i64() ),
            Type::UnsignedLongLong => quote!( $reader.read_u64() ),
            Type::Float => quote!( $reader.read_f32() ),
            Type::Double => quote!( $reader.read_f64() ),
            Type::LongDouble => quote!( static_cast<long double>($reader.read_f64()) ),
            Type::String { .. } => quote!( $reader.read_string() ),
            Type::Sequence { element_type, .. } => {
                let inner = self.deserialize_value(&element_type, scope, quote!(reader));
                quote! {
                    $reader.read_sequence([&](CdrReader &reader) {
                        return $inner;
                    })
                }
            }
            Type::Array {
                element_type,
                dimensions,
            } => {
                let mut elem = *element_type;
                for _ in dimensions {
                    elem = Type::Sequence {
                        element_type: Box::new(elem),
                        size: None,
                    };
                }
                self.deserialize_value(&elem, scope, reader)
            }
            Type::ScopedName(path) => {
                if let Some(base) = self.registry.enum_base(&path) {
                    let read_expr = match base {
                        Type::UnsignedShort => quote!(read_u16),
                        Type::Short => quote!(read_i16),
                        Type::UnsignedLong => quote!(read_u32),
                        Type::Long => quote!(read_i32),
                        Type::UnsignedLongLong => quote!(read_u64),
                        Type::LongLong => quote!(read_i64),
                        Type::Octet => quote!(read_u8),
                        Type::Boolean => quote!(read_bool),
                        Type::Char => quote!(read_char),
                        Type::Float => quote!(read_f32),
                        Type::Double => quote!(read_f64),
                        _ => quote!(read_u32),
                    };
                    let scoped = absolute_path(&path);
                    quote! {
                        static_cast<$scoped>($reader.$read_expr())
                    }
                } else {
                    let scoped = absolute_path(&path);
                    quote!( $scoped::read($reader) )
                }
            }
            Type::WString | Type::WChar => quote!(std::u16string()),
        }
    }
}

fn helpers_tokens() -> Tokens {
    quote! {
        inline void write_u16_raw(std::uint16_t value, std::uint8_t *out) {
          out[0] = static_cast<std::uint8_t>(value & 0xffu);
          out[1] = static_cast<std::uint8_t>((value >> 8) & 0xffu);
        }

        inline std::uint16_t read_u16_raw(const std::uint8_t *in) {
          return static_cast<std::uint16_t>(static_cast<std::uint16_t>(in[0]) |
                                           static_cast<std::uint16_t>(in[1] << 8));
        }

        inline void write_u32_raw(std::uint32_t value, std::uint8_t *out) {
          for (int i = 0; i < 4; ++i) {
            out[i] = static_cast<std::uint8_t>((value >> (8 * i)) & 0xffu);
          }
        }

        inline std::uint32_t read_u32_raw(const std::uint8_t *in) {
          return static_cast<std::uint32_t>(in[0]) | (static_cast<std::uint32_t>(in[1]) << 8) |
                 (static_cast<std::uint32_t>(in[2]) << 16) | (static_cast<std::uint32_t>(in[3]) << 24);
        }

        inline void write_u64_raw(std::uint64_t value, std::uint8_t *out) {
          for (int i = 0; i < 8; ++i) {
            out[i] = static_cast<std::uint8_t>((value >> (8 * i)) & 0xffu);
          }
        }

        inline std::uint64_t read_u64_raw(const std::uint8_t *in) {
          std::uint64_t result = 0;
          for (int i = 0; i < 8; ++i) {
            result |= static_cast<std::uint64_t>(in[i]) << (8 * i);
          }
          return result;
        }

        struct CdrWriter {
          std::vector<std::uint8_t> buffer;

          void align(std::size_t alignment) {
            if (alignment <= 1) { return; }
            const auto pad = (alignment - (buffer.size() % alignment)) % alignment;
            buffer.insert(buffer.end(), pad, 0);
          }

          void write_bool(bool value) {
            align(1);
            buffer.push_back(value ? 1u : 0u);
          }

          void write_char(char value) {
            align(1);
            buffer.push_back(static_cast<std::uint8_t>(value));
          }

          void write_u8(std::uint8_t value) {
            align(1);
            buffer.push_back(value);
          }

          void write_i16(std::int16_t value) {
            align(2);
            std::array<std::uint8_t, 2> bytes{};
            write_u16_raw(static_cast<std::uint16_t>(value), bytes.data());
            buffer.insert(buffer.end(), bytes.begin(), bytes.end());
          }

          void write_u16(std::uint16_t value) {
            align(2);
            std::array<std::uint8_t, 2> bytes{};
            write_u16_raw(value, bytes.data());
            buffer.insert(buffer.end(), bytes.begin(), bytes.end());
          }

          void write_i32(std::int32_t value) {
            align(4);
            std::array<std::uint8_t, 4> bytes{};
            write_u32_raw(static_cast<std::uint32_t>(value), bytes.data());
            buffer.insert(buffer.end(), bytes.begin(), bytes.end());
          }

          void write_u32(std::uint32_t value) {
            align(4);
            std::array<std::uint8_t, 4> bytes{};
            write_u32_raw(value, bytes.data());
            buffer.insert(buffer.end(), bytes.begin(), bytes.end());
          }

          void write_i64(std::int64_t value) {
            align(8);
            std::array<std::uint8_t, 8> bytes{};
            write_u64_raw(static_cast<std::uint64_t>(value), bytes.data());
            buffer.insert(buffer.end(), bytes.begin(), bytes.end());
          }

          void write_u64(std::uint64_t value) {
            align(8);
            std::array<std::uint8_t, 8> bytes{};
            write_u64_raw(value, bytes.data());
            buffer.insert(buffer.end(), bytes.begin(), bytes.end());
          }

          void write_f32(float value) {
            align(4);
            std::array<std::uint8_t, 4> bytes{};
            std::uint32_t bits;
            std::memcpy(&bits, &value, sizeof(bits));
            write_u32_raw(bits, bytes.data());
            buffer.insert(buffer.end(), bytes.begin(), bytes.end());
          }

          void write_f64(double value) {
            align(8);
            std::array<std::uint8_t, 8> bytes{};
            std::uint64_t bits;
            std::memcpy(&bits, &value, sizeof(bits));
            write_u64_raw(bits, bytes.data());
            buffer.insert(buffer.end(), bytes.begin(), bytes.end());
          }

          void write_string(std::string_view value) {
            align(4);
            const auto len = static_cast<std::uint32_t>(value.size() + 1);
            write_u32(len);
            buffer.insert(buffer.end(), value.begin(), value.end());
            buffer.push_back(0);
          }

          template <typename T, typename WriteFn>
          void write_sequence(const std::vector<T> &values, WriteFn &&write_fn) {
            align(4);
            write_u32(static_cast<std::uint32_t>(values.size()));
            for (const auto &item : values) {
              write_fn(*this, item);
            }
          }
        };

        struct CdrReader {
          std::span<const std::uint8_t> data;
          std::size_t offset{0};

          explicit CdrReader(std::span<const std::uint8_t> d) : data(d) {}

          void align(std::size_t alignment) {
            if (alignment <= 1) { return; }
            const auto pad = (alignment - (offset % alignment)) % alignment;
            if (offset + pad > data.size()) { throw std::runtime_error("cursor out of range"); }
            offset += pad;
          }

          std::span<const std::uint8_t> take(std::size_t size) {
            if (offset + size > data.size()) { throw std::runtime_error("cursor out of range"); }
            auto slice = data.subspan(offset, size);
            offset += size;
            return slice;
          }

          bool read_bool() {
            align(1);
            auto bytes = take(1);
            return bytes[0] != 0;
          }

          char read_char() {
            align(1);
            auto bytes = take(1);
            return static_cast<char>(bytes[0]);
          }

          std::uint8_t read_u8() {
            align(1);
            auto bytes = take(1);
            return bytes[0];
          }

          std::int16_t read_i16() {
            align(2);
            auto bytes = take(2);
            return static_cast<std::int16_t>(read_u16_raw(bytes.data()));
          }

          std::uint16_t read_u16() {
            align(2);
            auto bytes = take(2);
            return read_u16_raw(bytes.data());
          }

          std::int32_t read_i32() {
            align(4);
            auto bytes = take(4);
            return static_cast<std::int32_t>(read_u32_raw(bytes.data()));
          }

          std::uint32_t read_u32() {
            align(4);
            auto bytes = take(4);
            return read_u32_raw(bytes.data());
          }

          std::int64_t read_i64() {
            align(8);
            auto bytes = take(8);
            return static_cast<std::int64_t>(read_u64_raw(bytes.data()));
          }

          std::uint64_t read_u64() {
            align(8);
            auto bytes = take(8);
            return read_u64_raw(bytes.data());
          }

          float read_f32() {
            align(4);
            auto bytes = take(4);
            std::uint32_t bits = read_u32_raw(bytes.data());
            float value;
            std::memcpy(&value, &bits, sizeof(value));
            return value;
          }

          double read_f64() {
            align(8);
            auto bytes = take(8);
            std::uint64_t bits = read_u64_raw(bytes.data());
            double value;
            std::memcpy(&value, &bits, sizeof(value));
            return value;
          }

          std::string read_string() {
            align(4);
            auto len = read_u32();
            if (len == 0) { return std::string(); }
            auto bytes = take(len);
            if (bytes[len - 1] != 0) { throw std::runtime_error("CDR string missing null terminator"); }
            return std::string(reinterpret_cast<const char *>(bytes.data()), len - 1);
          }

          template <typename ReadFn>
          auto read_sequence(ReadFn &&read_fn) {
            align(4);
            auto len = read_u32();
            using Elem = std::invoke_result_t<ReadFn, CdrReader &>;
            std::vector<Elem> values;
            values.reserve(len);
            for (std::uint32_t i = 0; i < len; ++i) {
              values.push_back(read_fn(*this));
            }
            return values;
          }
        };

        inline std::string format_topic(std::string_view template_) {
          return std::string(template_);
        }
    }
}

fn const_literal(value: &ConstValue) -> Tokens {
    match value {
        ConstValue::Integer(lit) => quote!( $(lit.value) ),
        ConstValue::Float(v) => quote!( $(format!("{v}")) ),
        ConstValue::Fixed(f) => {
            let mut digits = f.digits.clone();
            if f.scale > 0 {
                let point = digits.len().saturating_sub(f.scale as usize);
                digits.insert(point, '.');
            }
            if f.negative {
                digits.insert(0, '-');
            }
            quote!( $(digits) )
        }
        ConstValue::Binary(bin) => {
            let value = bin.to_i64();
            quote!( $value )
        }
        ConstValue::String(s) => quote!( $(quoted_string(s)) ),
        ConstValue::Boolean(value) => {
            if *value {
                quote!(true)
            } else {
                quote!(false)
            }
        }
        ConstValue::Char(ch) => quote!( $(format!("'{}'", ch)) ),
        ConstValue::ScopedName(path) => quote!( $(path.join("::")) ),
        ConstValue::UnaryOp { .. } | ConstValue::BinaryOp { .. } => quote!(0),
    }
}

fn annotation_value<'a>(annotations: &'a [Annotation], name: &str) -> Option<&'a ConstValue> {
    annotations
        .iter()
        .find(|annotation| {
            annotation
                .name
                .last()
                .map(|segment| segment.eq_ignore_ascii_case(name))
                .unwrap_or(false)
        })
        .and_then(|annotation| {
            annotation
                .params
                .iter()
                .map(|param| match param {
                    AnnotationParam::Named { name, value }
                        if name.eq_ignore_ascii_case("value") =>
                    {
                        value
                    }
                    AnnotationParam::Positional(value) => value,
                    AnnotationParam::Named { value, .. } => value,
                })
                .next()
        })
}

fn annotation_string(annotations: &[Annotation], name: &str) -> Option<String> {
    annotation_value(annotations, name).and_then(|value| match value {
        ConstValue::String(value) => Some(value.clone()),
        _ => None,
    })
}

fn annotation_u16(annotations: &[Annotation], name: &str) -> Option<u16> {
    annotation_value(annotations, name).and_then(|value| match value {
        ConstValue::Integer(lit) if (0..=u16::MAX as i64).contains(&lit.value) => {
            Some(lit.value as u16)
        }
        _ => None,
    })
}

fn scoped_name(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
}

fn absolute_path(path: &[String]) -> String {
    format!("::blueberry_generated::{}", path.join("::"))
}

fn quoted_string(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    format!("\"{escaped}\"")
}

#[derive(Default)]
struct MessageKeyGen {
    next: u16,
}

impl MessageKeyGen {
    fn next(&mut self) -> u16 {
        self.next = self.next.wrapping_add(1);
        if self.next == 0 {
            self.next = 1;
        }
        self.next
    }
}
