use blueberry_ast::{
    Annotation, AnnotationParam, Commented, ConstDef, ConstValue, Definition, EnumDef, MessageDef,
    StructDef, Type, TypeDef,
};
use blueberry_codegen_core::{
    CodegenError, DEFAULT_MODULE_KEY, GeneratedFile, MESSAGE_HEADER_SIZE, ResolvedMember,
    TypeRegistry, class_name, quoted_string, uppercase_path,
};
use genco::lang::c::Tokens;
use genco::quote;

const RUNTIME_HEADER_PATH: &str = "c/blueberry_runtime.h";
const RUNTIME_SOURCE_PATH: &str = "c/blueberry_runtime.c";
const DEFINITIONS_HEADER_PATH: &str = "c/blueberry_messages.h";
const DEFINITIONS_SOURCE_PATH: &str = "c/blueberry_messages.c";

/// Generate the C runtime and data structures for the provided IDL definitions.
pub fn generate(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let mut generator = CGenerator::new(definitions);
    let runtime_header = CGenerator::render_runtime_header();
    let runtime_source = CGenerator::render_runtime_source();
    let header = generator.render_messages_header(definitions)?;
    let source = generator.render_messages_source();
    Ok(vec![
        GeneratedFile {
            path: RUNTIME_HEADER_PATH.to_string(),
            contents: runtime_header,
        },
        GeneratedFile {
            path: RUNTIME_SOURCE_PATH.to_string(),
            contents: runtime_source,
        },
        GeneratedFile {
            path: DEFINITIONS_HEADER_PATH.to_string(),
            contents: header,
        },
        GeneratedFile {
            path: DEFINITIONS_SOURCE_PATH.to_string(),
            contents: source,
        },
    ])
}

struct CGenerator {
    registry: TypeRegistry,
    message_topics: Vec<MessageTopic>,
}

struct MessageTopic {
    ident: String,
    literal: String,
}

impl CGenerator {
    fn new(definitions: &[Definition]) -> Self {
        Self {
            registry: TypeRegistry::new(definitions),
            message_topics: Vec::new(),
        }
    }

    fn render_messages_header(
        &mut self,
        definitions: &[Definition],
    ) -> Result<String, CodegenError> {
        self.message_topics.clear();
        let mut key_gen = MessageKeyGen::default();
        let definitions_tokens = self.emit_definitions(
            definitions,
            &mut Vec::new(),
            DEFAULT_MODULE_KEY,
            &mut key_gen,
        )?;
        let tokens: Tokens = quote! {
            #ifndef BLUEBERRY_MESSAGES_H
            #define BLUEBERRY_MESSAGES_H

            #include "blueberry_runtime.h"

            #ifdef __cplusplus
            extern "C" {
            #endif

            $(for section in definitions_tokens =>
                $section

            )

            #ifdef __cplusplus
            } // extern "C"
            #endif

            #endif /* BLUEBERRY_MESSAGES_H */
        };
        Ok(tokens.to_file_string().expect("render blueberry c header"))
    }

    fn render_messages_source(&self) -> String {
        let topics: Vec<Tokens> = self
            .message_topics
            .iter()
            .map(|topic| {
                let ident = &topic.ident;
                let literal = &topic.literal;
                quote!(const char $ident[] = $literal; $['\n'])
            })
            .collect();
        let tokens: Tokens = quote! {
            #include "blueberry_messages.h"
            #include <stdio.h>

            $(for entry in topics =>
                $entry

            )
        };
        tokens.to_file_string().expect("render blueberry c source")
    }

    fn render_runtime_header() -> String {
        let runtime_tokens = Self::runtime_header_tokens();
        let tokens: Tokens = quote! {
            #ifndef BLUEBERRY_RUNTIME_H
            #define BLUEBERRY_RUNTIME_H

            #include <stdbool.h>
            #include <stddef.h>
            #include <stdint.h>
            #include <string.h>

            #ifdef __cplusplus
            extern "C" {
            #endif

            $runtime_tokens

            #ifdef __cplusplus
            } // extern "C"
            #endif

            #endif /* BLUEBERRY_RUNTIME_H */
        };
        tokens
            .to_file_string()
            .expect("render blueberry runtime header")
    }

    fn render_runtime_source() -> String {
        let runtime_tokens = Self::runtime_source_tokens();
        let tokens: Tokens = quote! {
            #include "blueberry_runtime.h"

            $runtime_tokens
        };
        tokens
            .to_file_string()
            .expect("render blueberry runtime source")
    }

    fn runtime_header_tokens() -> Tokens {
        let header_size_literal = format!("((size_t){})", MESSAGE_HEADER_SIZE);
        quote! {
            #define BLUEBERRY_MESSAGE_HEADER_SIZE $header_size_literal

            typedef struct {
                uint16_t payload_words;
                uint16_t flags;
                uint16_t module_key;
                uint16_t message_key;
            } blueberry_message_header_t;

            bool blueberry_message_header_parse(
                const uint8_t *frame,
                size_t frame_len,
                blueberry_message_header_t *out_header);

            bool blueberry_message_header_write(
                uint8_t *frame,
                size_t frame_len,
                const blueberry_message_header_t *header);

            static inline size_t blueberry_align4(size_t value) {
                return (value + 3u) & ~((size_t)3u);
            }

            static inline uint16_t blueberry_payload_words_from_length(size_t length) {
                size_t aligned = blueberry_align4(length);
                if (aligned == 0u) {
                    return 0u;
                }
                return (uint16_t)(aligned / 4u);
            }

            static inline size_t blueberry_payload_length_from_words(uint16_t words) {
                return (size_t)words * 4u;
            }

            static inline size_t blueberry_frame_size_from_payload(size_t length) {
                return BLUEBERRY_MESSAGE_HEADER_SIZE + blueberry_align4(length);
            }

            static inline uint8_t blueberry_read_u8(const uint8_t *ptr) {
                return ptr[0];
            }

            static inline void blueberry_write_u8(uint8_t *ptr, uint8_t value) {
                ptr[0] = value;
            }

            static inline uint16_t blueberry_read_u16(const uint8_t *ptr) {
                return (uint16_t)ptr[0] | ((uint16_t)ptr[1] << 8);
            }

            static inline void blueberry_write_u16(uint8_t *ptr, uint16_t value) {
                ptr[0] = (uint8_t)(value & 0xffu);
                ptr[1] = (uint8_t)((value >> 8) & 0xffu);
            }

            static inline uint32_t blueberry_read_u32(const uint8_t *ptr) {
                return (uint32_t)ptr[0]
                    | ((uint32_t)ptr[1] << 8)
                    | ((uint32_t)ptr[2] << 16)
                    | ((uint32_t)ptr[3] << 24);
            }

            static inline void blueberry_write_u32(uint8_t *ptr, uint32_t value) {
                ptr[0] = (uint8_t)(value & 0xffu);
                ptr[1] = (uint8_t)((value >> 8) & 0xffu);
                ptr[2] = (uint8_t)((value >> 16) & 0xffu);
                ptr[3] = (uint8_t)((value >> 24) & 0xffu);
            }

            static inline uint64_t blueberry_read_u64(const uint8_t *ptr) {
                uint64_t value = 0u;
                value |= (uint64_t)ptr[0];
                value |= ((uint64_t)ptr[1] << 8);
                value |= ((uint64_t)ptr[2] << 16);
                value |= ((uint64_t)ptr[3] << 24);
                value |= ((uint64_t)ptr[4] << 32);
                value |= ((uint64_t)ptr[5] << 40);
                value |= ((uint64_t)ptr[6] << 48);
                value |= ((uint64_t)ptr[7] << 56);
                return value;
            }

            static inline void blueberry_write_u64(uint8_t *ptr, uint64_t value) {
                ptr[0] = (uint8_t)(value & 0xffu);
                ptr[1] = (uint8_t)((value >> 8) & 0xffu);
                ptr[2] = (uint8_t)((value >> 16) & 0xffu);
                ptr[3] = (uint8_t)((value >> 24) & 0xffu);
                ptr[4] = (uint8_t)((value >> 32) & 0xffu);
                ptr[5] = (uint8_t)((value >> 40) & 0xffu);
                ptr[6] = (uint8_t)((value >> 48) & 0xffu);
                ptr[7] = (uint8_t)((value >> 56) & 0xffu);
            }

            static inline bool blueberry_read_bool(const uint8_t *ptr) {
                return blueberry_read_u8(ptr) != 0u;
            }

            static inline void blueberry_write_bool(uint8_t *ptr, bool value) {
                blueberry_write_u8(ptr, value ? 1u : 0u);
            }

            static inline int8_t blueberry_read_char(const uint8_t *ptr) {
                uint8_t raw = blueberry_read_u8(ptr);
                int8_t value;
                memcpy(&value, &raw, sizeof(value));
                return value;
            }

            static inline void blueberry_write_char(uint8_t *ptr, int8_t value) {
                uint8_t raw;
                memcpy(&raw, &value, sizeof(raw));
                blueberry_write_u8(ptr, raw);
            }

            static inline int16_t blueberry_read_i16(const uint8_t *ptr) {
                uint16_t raw = blueberry_read_u16(ptr);
                int16_t value;
                memcpy(&value, &raw, sizeof(value));
                return value;
            }

            static inline void blueberry_write_i16(uint8_t *ptr, int16_t value) {
                uint16_t raw;
                memcpy(&raw, &value, sizeof(raw));
                blueberry_write_u16(ptr, raw);
            }

            static inline int32_t blueberry_read_i32(const uint8_t *ptr) {
                uint32_t raw = blueberry_read_u32(ptr);
                int32_t value;
                memcpy(&value, &raw, sizeof(value));
                return value;
            }

            static inline void blueberry_write_i32(uint8_t *ptr, int32_t value) {
                uint32_t raw;
                memcpy(&raw, &value, sizeof(raw));
                blueberry_write_u32(ptr, raw);
            }

            static inline int64_t blueberry_read_i64(const uint8_t *ptr) {
                uint64_t raw = blueberry_read_u64(ptr);
                int64_t value;
                memcpy(&value, &raw, sizeof(value));
                return value;
            }

            static inline void blueberry_write_i64(uint8_t *ptr, int64_t value) {
                uint64_t raw;
                memcpy(&raw, &value, sizeof(raw));
                blueberry_write_u64(ptr, raw);
            }

            static inline float blueberry_read_f32(const uint8_t *ptr) {
                uint32_t raw = blueberry_read_u32(ptr);
                float value;
                memcpy(&value, &raw, sizeof(value));
                return value;
            }

            static inline void blueberry_write_f32(uint8_t *ptr, float value) {
                uint32_t raw;
                memcpy(&raw, &value, sizeof(raw));
                blueberry_write_u32(ptr, raw);
            }

            static inline double blueberry_read_f64(const uint8_t *ptr) {
                uint64_t raw = blueberry_read_u64(ptr);
                double value;
                memcpy(&value, &raw, sizeof(value));
                return value;
            }

            static inline void blueberry_write_f64(uint8_t *ptr, double value) {
                uint64_t raw;
                memcpy(&raw, &value, sizeof(raw));
                blueberry_write_u64(ptr, raw);
            }
        }
    }

    fn runtime_source_tokens() -> Tokens {
        quote! {
            bool blueberry_message_header_parse(
                const uint8_t *frame,
                size_t frame_len,
                blueberry_message_header_t *out_header) {
                if (frame == NULL || out_header == NULL) {
                    return false;
                }
                if (frame_len < BLUEBERRY_MESSAGE_HEADER_SIZE) {
                    return false;
                }
                out_header->payload_words = blueberry_read_u16(frame);
                out_header->flags = blueberry_read_u16(frame + 2);
                out_header->module_key = blueberry_read_u16(frame + 4);
                out_header->message_key = blueberry_read_u16(frame + 6);
                return true;
            }

            bool blueberry_message_header_write(
                uint8_t *frame,
                size_t frame_len,
                const blueberry_message_header_t *header) {
                if (frame == NULL || header == NULL) {
                    return false;
                }
                if (frame_len < BLUEBERRY_MESSAGE_HEADER_SIZE) {
                    return false;
                }
                blueberry_write_u16(frame, header->payload_words);
                blueberry_write_u16(frame + 2, header->flags);
                blueberry_write_u16(frame + 4, header->module_key);
                blueberry_write_u16(frame + 6, header->message_key);
                return true;
            }
        }
    }

    fn emit_definitions(
        &mut self,
        defs: &[Definition],
        scope: &mut Vec<String>,
        module_key: u16,
        key_gen: &mut MessageKeyGen,
    ) -> Result<Vec<Tokens>, CodegenError> {
        let mut out = Vec::new();
        for def in defs {
            match def {
                Definition::ModuleDef(module_def) => {
                    let module_key =
                        annotation_u16(&module_def.annotations, "module_key").unwrap_or(module_key);
                    scope.push(module_def.node.name.clone());
                    let nested = self.emit_definitions(
                        &module_def.node.definitions,
                        scope,
                        module_key,
                        key_gen,
                    )?;
                    scope.pop();
                    out.extend(nested);
                }
                Definition::EnumDef(enum_def) => {
                    let enum_tokens = self.emit_enum(enum_def, scope);
                    out.push(quote! {
                        $enum_tokens

                    });
                }
                Definition::StructDef(struct_def) => {
                    let struct_tokens = self.emit_struct(struct_def, scope);
                    out.push(quote! {
                        $struct_tokens

                    });
                }
                Definition::MessageDef(message_def) => {
                    let message_tokens =
                        self.emit_message(message_def, scope, module_key, key_gen)?;
                    out.push(quote! {
                        $message_tokens

                    });
                }
                Definition::ConstDef(const_def) => {
                    let const_tokens = self.emit_const(const_def, scope);
                    out.push(quote! {
                        $const_tokens

                    });
                }
                Definition::TypeDef(typedef_def) => {
                    let typedef_tokens = self.emit_typedef(typedef_def, scope);
                    out.push(quote! {
                        $typedef_tokens

                    });
                }
                Definition::ImportDef(_) => {}
            }
        }
        Ok(out)
    }

    fn emit_enum(&self, enum_def: &Commented<EnumDef>, scope: &[String]) -> Tokens {
        let name = class_name(scope, &enum_def.node.name);
        let enumerators: Vec<Tokens> = enum_def
            .node
            .enumerators
            .iter()
            .map(|member| {
                if let Some(value) = &member.value {
                    let literal = Self::const_literal(value);
                    quote!( $(member.name.clone()) = $literal )
                } else {
                    quote!( $(member.name.clone()) )
                }
            })
            .collect();
        quote! {
            typedef enum {
                $(for value in enumerators join (,$['\n']) => $value)
            } $name;

            $['\n']
        }
    }

    fn emit_struct(&self, struct_def: &Commented<StructDef>, scope: &[String]) -> Tokens {
        let mut path = scope.to_vec();
        path.push(struct_def.node.name.clone());
        let members = self.registry.collect_struct_members(&path);
        self.render_struct_body(&class_name(scope, &struct_def.node.name), &members, scope)
    }

    fn emit_message(
        &mut self,
        message_def: &Commented<MessageDef>,
        scope: &[String],
        module_key: u16,
        key_gen: &mut MessageKeyGen,
    ) -> Result<Tokens, CodegenError> {
        let mut path = scope.to_vec();
        path.push(message_def.node.name.clone());
        let members = self.registry.collect_message_members(&path);
        let ident = class_name(scope, &message_def.node.name);
        let struct_tokens = self.render_struct_body(&ident, &members, scope);
        let scope_module_key =
            annotation_u16(&message_def.annotations, "module_key").unwrap_or(module_key);
        let message_key = annotation_u16(&message_def.annotations, "message_key")
            .unwrap_or_else(|| key_gen.next());
        let topic = annotation_string(&message_def.annotations, "topic").ok_or(
            CodegenError::MissingTopic {
                message: scoped_name(scope, &message_def.node.name),
            },
        )?;
        let upper = uppercase_path(scope, &message_def.node.name);
        let module_macro = format!("BLUEBERRY_{}_MODULE_KEY", upper);
        let message_macro = format!("BLUEBERRY_{}_MESSAGE_KEY", upper);
        let topic_ident = format!("BLUEBERRY_{}_TOPIC_TEMPLATE", upper);
        let module_literal = format!("((uint16_t)0x{scope_module_key:04X})");
        let message_literal = format!("((uint16_t)0x{message_key:04X})");
        let topic_literal = quoted_string(&topic);
        self.message_topics.push(MessageTopic {
            ident: topic_ident.clone(),
            literal: topic_literal.clone(),
        });
        Ok(quote! {
            $struct_tokens

            #define $module_macro $module_literal
            #define $message_macro $message_literal
            extern const char $topic_ident[];

            $['\n']
        })
    }

    fn emit_const(&self, const_def: &Commented<ConstDef>, scope: &[String]) -> Tokens {
        let name = class_name(scope, &const_def.node.name);
        let value = Self::const_literal(&const_def.node.value);
        match &const_def.node.const_type {
            Type::String { .. } => quote!(static const char $name[] = $value; $['\n']),
            Type::WString => quote!(static const uint16_t $name[] = $value; $['\n']),
            Type::Char => quote!(static const int8_t $name = $value; $['\n']),
            Type::WChar => quote!(static const uint16_t $name = $value; $['\n']),
            other => {
                let resolved = self.registry.resolve_type(other, scope);
                let ty = self.type_tokens(&resolved, scope);
                quote!(static const $ty $name = $value; $['\n'])
            }
        }
    }

    fn emit_typedef(&self, typedef_def: &Commented<TypeDef>, scope: &[String]) -> Tokens {
        let name = class_name(scope, &typedef_def.node.name);
        match &typedef_def.node.base_type {
            Type::Array {
                element_type,
                dimensions,
            } => {
                let resolved = self.registry.resolve_type(element_type, scope);
                let base = self.type_tokens(&resolved, scope);
                let dims: Vec<String> = dimensions.iter().map(|dim| format!("[{}]", dim)).collect();
                quote! {
                    typedef $base $name$(for dim in dims => $dim);

                    $['\n']
                }
            }
            other => {
                let resolved = self.registry.resolve_type(other, scope);
                let ty = self.type_tokens(&resolved, scope);
                quote!(typedef $ty $name; $['\n'])
            }
        }
    }

    fn render_struct_body(
        &self,
        ident: &str,
        members: &[ResolvedMember],
        scope: &[String],
    ) -> Tokens {
        let fields: Vec<Tokens> = members
            .iter()
            .map(|member| self.field_tokens(&member.ty, &member.name, scope))
            .collect();
        quote! {
            typedef struct {
                $(for field in fields =>
                    $field
                )
            } $ident;

            $['\n']
        }
    }

    fn field_tokens(&self, ty: &Type, name: &str, scope: &[String]) -> Tokens {
        let ty_tokens = self.type_tokens(ty, scope);
        quote!( $ty_tokens $name; $['\n'] )
    }

    fn type_tokens(&self, ty: &Type, scope: &[String]) -> Tokens {
        match ty {
            Type::Long => quote!(int32_t),
            Type::Short => quote!(int16_t),
            Type::UnsignedShort => quote!(uint16_t),
            Type::UnsignedLong => quote!(uint32_t),
            Type::LongLong => quote!(int64_t),
            Type::UnsignedLongLong => quote!(uint64_t),
            Type::Float => quote!(float),
            Type::Double => quote!(double),
            Type::LongDouble => quote!(long double),
            Type::Boolean => quote!(bool),
            Type::Octet => quote!(uint8_t),
            Type::Char => quote!(int8_t),
            Type::WChar => quote!(uint16_t),
            Type::String { bound } => {
                if let Some(bound) = bound {
                    let bound_lit = format!("{}", bound);
                    quote! {
                        struct {
                            size_t len;
                            char data[$bound_lit];
                        }
                    }
                } else {
                    quote! {
                        struct {
                            size_t len;
                            char *data;
                        }
                    }
                }
            }
            Type::WString => quote! {
                struct {
                    size_t len;
                    uint16_t *data;
                }
            },
            Type::Sequence { element_type, size } => {
                let element = self.type_tokens(element_type, scope);
                if let Some(bound) = size {
                    let bound_lit = format!("{}", bound);
                    quote! {
                        struct {
                            size_t len;
                            $element items[$bound_lit];
                        }
                    }
                } else {
                    quote! {
                        struct {
                            size_t len;
                            $element *items;
                        }
                    }
                }
            }
            Type::ScopedName(path) => {
                let ident = c_path_name(path);
                quote!( $ident )
            }
            Type::Array {
                element_type,
                dimensions,
            } => {
                let resolved = self.registry.resolve_type(element_type, scope);
                let base = self.type_tokens(&resolved, scope);
                let dims: Vec<String> = dimensions.iter().map(|dim| format!("[{}]", dim)).collect();
                quote!( $base$(for dim in dims => $dim) )
            }
        }
    }

    fn const_literal(value: &ConstValue) -> Tokens {
        match value {
            ConstValue::Integer(lit) => quote!($(lit.value)),
            ConstValue::Float(f) => quote!($(format!("{}", f))),
            ConstValue::Fixed(fixed) => {
                let mut digits = fixed.digits.clone();
                if fixed.scale > 0 {
                    let point = digits.len().saturating_sub(fixed.scale as usize);
                    digits.insert(point, '.');
                }
                if fixed.negative {
                    digits.insert(0, '-');
                }
                quote!( $(digits) )
            }
            ConstValue::Binary(binary) => {
                let literal = binary.to_i128();
                quote!( $literal )
            }
            ConstValue::String(text) => quote!( $(quoted_string(text)) ),
            ConstValue::Boolean(true) => quote!(true),
            ConstValue::Boolean(false) => quote!(false),
            ConstValue::Char(ch) => {
                let escaped = ch.escape_default().to_string();
                quote!( $(format!("'{}'", escaped)) )
            }
            ConstValue::ScopedName(path) => quote!( $(c_path_name(path)) ),
            ConstValue::UnaryOp { op, expr } => {
                let inner = Self::const_literal(expr);
                let op_token = match op {
                    blueberry_ast::UnaryOperator::Plus => quote!(+),
                    blueberry_ast::UnaryOperator::Minus => quote!(-),
                };
                quote!(( $op_token $inner ))
            }
            ConstValue::BinaryOp { op, left, right } => {
                let lhs = Self::const_literal(left);
                let rhs = Self::const_literal(right);
                let op_token = match op {
                    blueberry_ast::BinaryOperator::Add => quote!(+),
                    blueberry_ast::BinaryOperator::Subtract => quote!(-),
                    blueberry_ast::BinaryOperator::Multiply => quote!(*),
                    blueberry_ast::BinaryOperator::Divide => quote!(/),
                };
                quote!(( $lhs $op_token $rhs ))
            }
        }
    }
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

fn c_path_name(path: &[String]) -> String {
    path.join("_")
}

fn scoped_name(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
}

fn annotation_string(annotations: &[Annotation], name: &str) -> Option<String> {
    annotation_value(annotations, name).and_then(|value| match value {
        ConstValue::String(value) => Some(value.clone()),
        _ => None,
    })
}

fn annotation_u16(annotations: &[Annotation], name: &str) -> Option<u16> {
    annotation_value(annotations, name).and_then(|value| match value {
        ConstValue::Integer(lit) if (0..=u16::MAX as i128).contains(&lit.value) => {
            Some(lit.value as u16)
        }
        _ => None,
    })
}

fn annotation_value<'a>(annotations: &'a [Annotation], name: &str) -> Option<&'a ConstValue> {
    annotations
        .iter()
        .find(|annotation| annotation_name_matches(annotation, name))
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

fn annotation_name_matches(annotation: &Annotation, expected: &str) -> bool {
    annotation
        .name
        .last()
        .map(|segment| segment.eq_ignore_ascii_case(expected))
        .unwrap_or(false)
}
