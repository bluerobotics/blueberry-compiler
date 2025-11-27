use blueberry_ast::Definition;
use blueberry_codegen_core::{
    CodegenError, GeneratedFile, MessageSpec, PrimitiveType, class_name, collect_messages,
    quoted_string,
};
use genco::lang::c::Tokens;
use genco::quote;

const OUTPUT_PATH: &str = "cpp/messages.hpp";

pub fn generate(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let messages = collect_messages(definitions)?;
    if messages.is_empty() {
        return Ok(Vec::new());
    }
    let generator = CppGenerator::new(&messages);
    let contents = generator.render();
    Ok(vec![GeneratedFile {
        path: OUTPUT_PATH.to_string(),
        contents,
    }])
}

struct CppGenerator<'a> {
    messages: &'a [MessageSpec],
}

impl<'a> CppGenerator<'a> {
    fn new(messages: &'a [MessageSpec]) -> Self {
        Self { messages }
    }

    fn render(&self) -> String {
        let message_blocks: Vec<Tokens> =
            self.messages.iter().map(|m| self.emit_message(m)).collect();

        let helpers = helpers_tokens();
        let header = message_header();
        let tokens: Tokens = quote! {
            #pragma once

            #include <array>
            #include <cstdint>
            #include <cstring>
            #include <span>
            #include <stdexcept>
            #include <string>
            #include <string_view>
            #include <utility>

            namespace blueberry::messages {

            $helpers

            $header

            $(for block in &message_blocks =>
                $block

            )

            } // namespace blueberry::messages
        };

        tokens.to_file_string().expect("render cpp output")
    }

    fn emit_message(&self, message: &MessageSpec) -> Tokens {
        let class_ident = class_name(&message.scope, &message.name);
        let class_ident_ref = class_ident.as_str();
        let topic_literal = quoted_string(&message.topic);
        let module_key = message.module_key;
        let message_key = message.message_key;
        let struct_size = message.field_payload_size();
        let padded_size = message.padded_payload_size();
        let payload_words = message.payload_words();

        let fields = &message.fields;
        let ctor_params: Tokens = quote! {
            $(for field in fields join (, ) =>
                $(cpp_field_type(field.primitive)) $(field.name.clone()) = $(cpp_default(field.primitive))
            )
        };
        let ctor_inits: Tokens = quote! {
            $(for field in fields join (, ) =>
                $(field.name.clone())_($(field.name.clone()))
            )
        };

        let accessors: Tokens = quote! {
            $(for field in fields =>
                [[nodiscard]] $(cpp_field_type(field.primitive)) $(field.name.clone())() const noexcept { return $(field.name.clone())_; }
                void set_$(field.name.clone())($(cpp_field_type(field.primitive)) value) noexcept { $(field.name.clone())_ = value; }
            )
        };

        let serialize_body: Tokens = quote! {
            std::array<std::uint8_t, kPaddedSize> payload{};
            std::size_t offset = 0;
            $(for field in fields =>
                $(cpp_writer(field.primitive))($(field.name.clone())_, payload.data() + offset);
                offset += $(field.primitive.size());
            )
            return payload;
        };

        let deserialize_body: Tokens = if fields.is_empty() {
            quote! { return $class_ident_ref(); }
        } else {
            let values: Tokens = quote! {
                $(for field in fields =>
                    auto value_$(field.name.clone()) = $(cpp_reader(field.primitive))(payload.data() + offset);
                    offset += $(field.primitive.size());
                )
            };
            let args: Tokens =
                quote! { $(for field in fields join (, ) => value_$(field.name.clone())) };
            quote! {
                std::size_t offset = 0;
                $values
                return $class_ident_ref($args);
            }
        };

        let members: Tokens = quote! {
            $(for field in fields =>
                $(cpp_field_type(field.primitive)) $(field.name.clone())_{};$['\n']
            )
        };

        quote! {
            class $class_ident_ref {
            public:
                static constexpr std::uint16_t kModuleKey = $module_key;
                static constexpr std::uint16_t kMessageKey = $message_key;
                static constexpr std::size_t kStructSize = $struct_size;
                static constexpr std::size_t kPaddedSize = $padded_size;
                static constexpr std::size_t kPayloadWords = $payload_words;
                static constexpr std::string_view kTopicTemplate = $topic_literal;

                $class_ident_ref() = default;
                $(if !fields.is_empty() {
                    explicit $class_ident_ref($ctor_params) : $ctor_inits {}
                })

                $accessors

                [[nodiscard]] std::array<std::uint8_t, kPaddedSize> Serialize() const {
                    $serialize_body
                }

                [[nodiscard]] static $class_ident_ref Deserialize(std::span<const std::uint8_t> payload) {
                    if (payload.size() < kStructSize) { throw std::runtime_error("payload shorter than struct"); }
                    $deserialize_body
                }

                [[nodiscard]] static $class_ident_ref FromFrame(std::span<const std::uint8_t> frame) {
                    auto header = MessageHeader::Parse(frame);
                    const auto payload_len = static_cast<std::size_t>(header.payload_words) * 4u;
                    if (frame.size() < MessageHeader::kSize + payload_len) { throw std::runtime_error("frame shorter than payload"); }
                    auto payload = frame.subspan(MessageHeader::kSize, payload_len);
                    return Deserialize(payload);
                }

                template <typename Session>
                void Publish(Session &session, std::string_view device_type, std::string_view nid) const {
                    auto payload = Serialize();
                    MessageHeader header{static_cast<std::uint16_t>(kPayloadWords), 0, kModuleKey, kMessageKey};
                    auto header_bytes = header.Pack();
                    std::array<std::uint8_t, MessageHeader::kSize + kPaddedSize> frame{};
                    std::memcpy(frame.data(), header_bytes.data(), header_bytes.size());
                    if constexpr (kPaddedSize > 0) { std::memcpy(frame.data() + MessageHeader::kSize, payload.data(), payload.size()); }
                    session.put(FormatTopic(device_type, nid), std::span<const std::uint8_t>(frame.data(), frame.size()));
                }

                template <typename Session, typename Callback>
                static void Subscribe(Session &session, std::string_view device_type, std::string_view nid, Callback &&callback) {
                    session.subscribe(FormatTopic(device_type, nid), [cb = std::forward<Callback>(callback)](std::span<const std::uint8_t> frame) { cb(FromFrame(frame)); });
                }

            private:
                static std::string FormatTopic(std::string_view device_type, std::string_view nid) {
                    std::string topic = std::string(kTopicTemplate);
                    replace_placeholder(topic, "{{device_type}}", device_type);
                    replace_placeholder(topic, "{{nid}}", nid);
                    return topic;
                }

                $members
            };
        }
    }
}

fn helpers_tokens() -> Tokens {
    quote! {
        inline void write_u16(std::uint16_t value, std::uint8_t *out) {
          out[0] = static_cast<std::uint8_t>(value & 0xffu);
          out[1] = static_cast<std::uint8_t>((value >> 8) & 0xffu);
        }

        inline std::uint16_t read_u16(const std::uint8_t *in) {
          return static_cast<std::uint16_t>(static_cast<std::uint16_t>(in[0]) |
                                           static_cast<std::uint16_t>(in[1] << 8));
        }

        inline void write_u32(std::uint32_t value, std::uint8_t *out) {
          for (int i = 0; i < 4; ++i) {
            out[i] = static_cast<std::uint8_t>((value >> (8 * i)) & 0xffu);
          }
        }

        inline std::uint32_t read_u32(const std::uint8_t *in) {
          return static_cast<std::uint32_t>(in[0]) | (static_cast<std::uint32_t>(in[1]) << 8) |
                 (static_cast<std::uint32_t>(in[2]) << 16) | (static_cast<std::uint32_t>(in[3]) << 24);
        }

        inline void write_u64(std::uint64_t value, std::uint8_t *out) {
          for (int i = 0; i < 8; ++i) {
            out[i] = static_cast<std::uint8_t>((value >> (8 * i)) & 0xffu);
          }
        }

        inline std::uint64_t read_u64(const std::uint8_t *in) {
          std::uint64_t result = 0;
          for (int i = 0; i < 8; ++i) {
            result |= static_cast<std::uint64_t>(in[i]) << (8 * i);
          }
          return result;
        }

        inline void write_bool(bool value, std::uint8_t *out) { out[0] = value ? 1u : 0u; }
        inline bool read_bool(const std::uint8_t *in) { return in[0] != 0; }
        inline void write_char(char value, std::uint8_t *out) { out[0] = static_cast<std::uint8_t>(value); }
        inline char read_char(const std::uint8_t *in) { return static_cast<char>(in[0]); }
        inline void write_u8(std::uint8_t value, std::uint8_t *out) { out[0] = value; }
        inline std::uint8_t read_u8(const std::uint8_t *in) { return in[0]; }
        inline void write_i16(std::int16_t value, std::uint8_t *out) { write_u16(static_cast<std::uint16_t>(value), out); }
        inline std::int16_t read_i16(const std::uint8_t *in) { return static_cast<std::int16_t>(read_u16(in)); }
        inline void write_u16_val(std::uint16_t value, std::uint8_t *out) { write_u16(value, out); }
        inline std::uint16_t read_u16_val(const std::uint8_t *in) { return read_u16(in); }
        inline void write_i32(std::int32_t value, std::uint8_t *out) { write_u32(static_cast<std::uint32_t>(value), out); }
        inline std::int32_t read_i32(const std::uint8_t *in) { return static_cast<std::int32_t>(read_u32(in)); }
        inline void write_u32_val(std::uint32_t value, std::uint8_t *out) { write_u32(value, out); }
        inline std::uint32_t read_u32_val(const std::uint8_t *in) { return read_u32(in); }
        inline void write_i64(std::int64_t value, std::uint8_t *out) { write_u64(static_cast<std::uint64_t>(value), out); }
        inline std::int64_t read_i64(const std::uint8_t *in) { return static_cast<std::int64_t>(read_u64(in)); }
        inline void write_u64_val(std::uint64_t value, std::uint8_t *out) { write_u64(value, out); }
        inline std::uint64_t read_u64_val(const std::uint8_t *in) { return read_u64(in); }

        inline void write_f32(float value, std::uint8_t *out) {
          std::uint32_t bits;
          std::memcpy(&bits, &value, sizeof(bits));
          write_u32(bits, out);
        }

        inline float read_f32(const std::uint8_t *in) {
          std::uint32_t bits = read_u32(in);
          float value;
          std::memcpy(&value, &bits, sizeof(value));
          return value;
        }

        inline void write_f64(double value, std::uint8_t *out) {
          std::uint64_t bits;
          std::memcpy(&bits, &value, sizeof(bits));
          write_u64(bits, out);
        }

        inline double read_f64(const std::uint8_t *in) {
          std::uint64_t bits = read_u64(in);
          double value;
          std::memcpy(&value, &bits, sizeof(value));
          return value;
        }

        inline void replace_placeholder(std::string &topic, std::string_view placeholder, std::string_view value) {
          const std::string needle(placeholder);
          const std::string replacement(value);
          std::size_t pos = topic.find(needle);
          while (pos != std::string::npos) {
            topic.replace(pos, needle.size(), replacement);
            pos = topic.find(needle, pos + replacement.size());
          }
        }
    }
}

fn message_header() -> Tokens {
    quote! {
        struct MessageHeader {
          std::uint16_t payload_words{};
          std::uint16_t flags{};
          std::uint16_t module_key{};
          std::uint16_t message_key{};

          static constexpr std::size_t kSize = 8;

          [[nodiscard]] std::array<std::uint8_t, kSize> Pack() const {
            std::array<std::uint8_t, kSize> data{};
            write_u16(payload_words, data.data());
            write_u16(flags, data.data() + 2);
            write_u16(module_key, data.data() + 4);
            write_u16(message_key, data.data() + 6);
            return data;
          }

          [[nodiscard]] static MessageHeader Parse(std::span<const std::uint8_t> frame) {
            if (frame.size() < static_cast<std::size_t>(kSize)) {
              throw std::runtime_error("frame shorter than header");
            }
            MessageHeader header{};
            header.payload_words = read_u16(frame.data());
            header.flags = read_u16(frame.data() + 2);
            header.module_key = read_u16(frame.data() + 4);
            header.message_key = read_u16(frame.data() + 6);
            return header;
          }
        };
    }
}

fn cpp_field_type(primitive: PrimitiveType) -> Tokens {
    match primitive {
        PrimitiveType::Bool => quote!(bool),
        PrimitiveType::Char => quote!(char),
        PrimitiveType::Octet => quote!(std::uint8_t),
        PrimitiveType::I16 => quote!(std::int16_t),
        PrimitiveType::U16 => quote!(std::uint16_t),
        PrimitiveType::I32 => quote!(std::int32_t),
        PrimitiveType::U32 => quote!(std::uint32_t),
        PrimitiveType::I64 => quote!(std::int64_t),
        PrimitiveType::U64 => quote!(std::uint64_t),
        PrimitiveType::F32 => quote!(float),
        PrimitiveType::F64 => quote!(double),
    }
}

fn cpp_default(primitive: PrimitiveType) -> Tokens {
    match primitive {
        PrimitiveType::Bool => quote!(false),
        PrimitiveType::Char => quote!('\0'),
        PrimitiveType::Octet
        | PrimitiveType::I16
        | PrimitiveType::U16
        | PrimitiveType::I32
        | PrimitiveType::U32
        | PrimitiveType::I64
        | PrimitiveType::U64 => quote!(0),
        PrimitiveType::F32 => quote!(0.0F),
        PrimitiveType::F64 => quote!(0.0),
    }
}

fn cpp_writer(primitive: PrimitiveType) -> Tokens {
    match primitive {
        PrimitiveType::Bool => quote!(write_bool),
        PrimitiveType::Char => quote!(write_char),
        PrimitiveType::Octet => quote!(write_u8),
        PrimitiveType::I16 => quote!(write_i16),
        PrimitiveType::U16 => quote!(write_u16_val),
        PrimitiveType::I32 => quote!(write_i32),
        PrimitiveType::U32 => quote!(write_u32_val),
        PrimitiveType::I64 => quote!(write_i64),
        PrimitiveType::U64 => quote!(write_u64_val),
        PrimitiveType::F32 => quote!(write_f32),
        PrimitiveType::F64 => quote!(write_f64),
    }
}

fn cpp_reader(primitive: PrimitiveType) -> Tokens {
    match primitive {
        PrimitiveType::Bool => quote!(read_bool),
        PrimitiveType::Char => quote!(read_char),
        PrimitiveType::Octet => quote!(read_u8),
        PrimitiveType::I16 => quote!(read_i16),
        PrimitiveType::U16 => quote!(read_u16),
        PrimitiveType::I32 => quote!(read_i32),
        PrimitiveType::U32 => quote!(read_u32),
        PrimitiveType::I64 => quote!(read_i64),
        PrimitiveType::U64 => quote!(read_u64),
        PrimitiveType::F32 => quote!(read_f32),
        PrimitiveType::F64 => quote!(read_f64),
    }
}
