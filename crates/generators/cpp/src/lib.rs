use blueberry_ast::Definition;
use blueberry_codegen_core::{
    CodegenError, GeneratedFile, MessageSpec, PrimitiveType, SourceWriter, class_name,
    collect_messages,
};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

const OUTPUT_PATH: &str = "cpp/messages.hpp";

pub fn generate(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let messages = collect_messages(definitions)?;
    if messages.is_empty() {
        return Ok(Vec::new());
    }
    let contents = render(&messages);
    Ok(vec![GeneratedFile {
        path: OUTPUT_PATH.to_string(),
        contents,
    }])
}

fn render(messages: &[MessageSpec]) -> String {
    let mut writer = SourceWriter::new();
    writer.push_str("#pragma once\n\n");
    writer.push_str("#include <array>\n");
    writer.push_str("#include <cstdint>\n");
    writer.push_str("#include <cstring>\n");
    writer.push_str("#include <span>\n");
    writer.push_str("#include <stdexcept>\n");
    writer.push_str("#include <string>\n");
    writer.push_str("#include <string_view>\n");
    writer.push_str("#include <utility>\n\n");
    writer.push_str("namespace blueberry::messages {\n\n");
    writer.push_str(CPP_HELPERS);
    writer.push_str(MESSAGE_HEADER);
    writer.newline();
    for message in messages {
        render_message(&mut writer, message);
    }
    writer.push_str("}\n");
    writer.into_string()
}

fn render_message(writer: &mut SourceWriter, message: &MessageSpec) {
    let class_ident = format_ident!("{}", class_name(&message.scope, &message.name));
    let topic_literal = Literal::string(&message.topic);
    let module_key = Literal::u16_unsuffixed(message.module_key);
    let message_key = Literal::u16_unsuffixed(message.message_key);
    let struct_size = Literal::usize_unsuffixed(message.field_payload_size());
    let padded_size = Literal::usize_unsuffixed(message.padded_payload_size());
    let payload_words = Literal::usize_unsuffixed(message.payload_words());
    start_block(writer, 0, quote! { class #class_ident });
    indent(writer, 1, quote! { public: });
    indent(
        writer,
        2,
        quote! { static constexpr std::uint16_t kModuleKey = #module_key; },
    );
    indent(
        writer,
        2,
        quote! { static constexpr std::uint16_t kMessageKey = #message_key; },
    );
    indent(
        writer,
        2,
        quote! { static constexpr std::size_t kStructSize = #struct_size; },
    );
    indent(
        writer,
        2,
        quote! { static constexpr std::size_t kPaddedSize = #padded_size; },
    );
    indent(
        writer,
        2,
        quote! { static constexpr std::size_t kPayloadWords = #payload_words; },
    );
    indent(
        writer,
        2,
        quote! { static constexpr std::string_view kTopicTemplate = #topic_literal; },
    );
    writer.newline();
    indent(writer, 2, quote! { #class_ident() = default; });
    if !message.fields.is_empty() {
        let params: Vec<TokenStream> = message
            .fields
            .iter()
            .map(|field| {
                let ty = cpp_field_type(field.primitive);
                let name = format_ident!("{}", field.name);
                let default = cpp_default(field.primitive);
                quote!(#ty #name = #default)
            })
            .collect();
        let initializers: Vec<TokenStream> = message
            .fields
            .iter()
            .map(|field| {
                let name = format_ident!("{}", field.name);
                let member = format_ident!("{}_", field.name);
                quote!(#member(#name))
            })
            .collect();
        indent(
            writer,
            2,
            quote! { explicit #class_ident(#(#params),*) : #(#initializers),* {} },
        );
    }
    for field in &message.fields {
        let ty = cpp_field_type(field.primitive);
        let name = format_ident!("{}", field.name);
        let setter = format_ident!("set_{}", field.name);
        let member = format_ident!("{}_", field.name);
        indent(
            writer,
            2,
            quote! { [[nodiscard]] #ty #name() const noexcept { return #member; } },
        );
        indent(
            writer,
            2,
            quote! { void #setter(#ty value) noexcept { #member = value; } },
        );
    }
    writer.newline();
    start_block(
        writer,
        2,
        quote! { [[nodiscard]] std::array<std::uint8_t, kPaddedSize> Serialize() const },
    );
    indent(
        writer,
        3,
        quote! { std::array<std::uint8_t, kPaddedSize> payload{}; },
    );
    indent(writer, 3, quote! { std::size_t offset = 0; });
    for field in &message.fields {
        let writer_fn = cpp_writer(field.primitive);
        let member = format_ident!("{}_", field.name);
        let size = Literal::usize_unsuffixed(field.primitive.size());
        indent(
            writer,
            3,
            quote! { #writer_fn(#member, payload.data() + offset); },
        );
        indent(writer, 3, quote! { offset += #size; });
    }
    indent(writer, 3, quote! { return payload; });
    close_block(writer, 2);
    writer.newline();

    start_block(
        writer,
        2,
        quote! { [[nodiscard]] static #class_ident Deserialize(std::span<const std::uint8_t> payload) },
    );
    indent(
        writer,
        3,
        quote! { if (payload.size() < kStructSize) { throw std::runtime_error("payload shorter than struct"); } },
    );
    if message.fields.is_empty() {
        indent(writer, 3, quote! { return #class_ident(); });
    } else {
        indent(writer, 3, quote! { std::size_t offset = 0; });
        for field in &message.fields {
            let tmp_name = format_ident!("value_{}", field.name);
            let reader = cpp_reader(field.primitive);
            let size = Literal::usize_unsuffixed(field.primitive.size());
            indent(
                writer,
                3,
                quote! { auto #tmp_name = #reader(payload.data() + offset); },
            );
            indent(writer, 3, quote! { offset += #size; });
        }
        let params: Vec<TokenStream> = message
            .fields
            .iter()
            .map(|field| {
                let tmp_name = format_ident!("value_{}", field.name);
                quote!(#tmp_name)
            })
            .collect();
        indent(writer, 3, quote! { return #class_ident(#(#params),*); });
    }
    close_block(writer, 2);
    writer.newline();

    start_block(
        writer,
        2,
        quote! { [[nodiscard]] static #class_ident FromFrame(std::span<const std::uint8_t> frame) },
    );
    indent(
        writer,
        3,
        quote! { auto header = MessageHeader::Parse(frame); },
    );
    indent(
        writer,
        3,
        quote! { const auto payload_len = static_cast<std::size_t>(header.payload_words) * 4u; },
    );
    indent(
        writer,
        3,
        quote! { if (frame.size() < MessageHeader::kSize + payload_len) { throw std::runtime_error("frame shorter than payload"); } },
    );
    indent(
        writer,
        3,
        quote! { auto payload = frame.subspan(MessageHeader::kSize, payload_len); },
    );
    indent(writer, 3, quote! { return Deserialize(payload); });
    close_block(writer, 2);
    writer.newline();

    indent(writer, 2, quote! { template <typename Session> });
    start_block(
        writer,
        2,
        quote! { void Publish(Session &session, std::string_view device_type, std::string_view nid) const },
    );
    indent(writer, 3, quote! { auto payload = Serialize(); });
    indent(
        writer,
        3,
        quote! { MessageHeader header{static_cast<std::uint16_t>(kPayloadWords), 0, kModuleKey, kMessageKey}; },
    );
    indent(writer, 3, quote! { auto header_bytes = header.Pack(); });
    indent(
        writer,
        3,
        quote! { std::array<std::uint8_t, MessageHeader::kSize + kPaddedSize> frame{}; },
    );
    indent(
        writer,
        3,
        quote! { std::memcpy(frame.data(), header_bytes.data(), header_bytes.size()); },
    );
    indent(
        writer,
        3,
        quote! { if constexpr (kPaddedSize > 0) { std::memcpy(frame.data() + MessageHeader::kSize, payload.data(), payload.size()); } },
    );
    indent(
        writer,
        3,
        quote! { session.put(FormatTopic(device_type, nid), std::span<const std::uint8_t>(frame.data(), frame.size())); },
    );
    close_block(writer, 2);
    writer.newline();

    indent(
        writer,
        2,
        quote! { template <typename Session, typename Callback> },
    );
    start_block(
        writer,
        2,
        quote! { static void Subscribe(Session &session, std::string_view device_type, std::string_view nid, Callback &&callback) },
    );
    indent(
        writer,
        3,
        quote! { session.subscribe(FormatTopic(device_type, nid), [cb = std::forward<Callback>(callback)](std::span<const std::uint8_t> frame) { cb(FromFrame(frame)); }); },
    );
    close_block(writer, 2);
    writer.newline();

    indent(writer, 1, quote! { private: });
    start_block(
        writer,
        2,
        quote! { static std::string FormatTopic(std::string_view device_type, std::string_view nid) },
    );
    indent(
        writer,
        3,
        quote! { std::string topic = std::string(kTopicTemplate); },
    );
    indent(
        writer,
        3,
        quote! { replace_placeholder(topic, "{{device_type}}", device_type); },
    );
    indent(
        writer,
        3,
        quote! { replace_placeholder(topic, "{{nid}}", nid); },
    );
    indent(writer, 3, quote! { return topic; });
    close_block(writer, 2);
    for field in &message.fields {
        let ty = cpp_field_type(field.primitive);
        let member = format_ident!("{}_", field.name);
        indent(writer, 2, quote! { #ty #member{}; });
    }
    writer.push_str("};\n\n");
}

fn indent(writer: &mut SourceWriter, level: usize, tokens: TokenStream) {
    for _ in 0..level {
        writer.push_str("  ");
    }
    writer.push_line(tokens);
}

fn start_block(writer: &mut SourceWriter, level: usize, header: TokenStream) {
    for _ in 0..level {
        writer.push_str("  ");
    }
    writer.push(header);
    writer.push_str(" {\n");
}

fn close_block(writer: &mut SourceWriter, level: usize) {
    for _ in 0..level {
        writer.push_str("  ");
    }
    writer.push_str("}\n");
}

fn cpp_field_type(primitive: PrimitiveType) -> TokenStream {
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

fn cpp_default(primitive: PrimitiveType) -> TokenStream {
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

fn cpp_writer(primitive: PrimitiveType) -> TokenStream {
    match primitive {
        PrimitiveType::Bool => quote!(blueberry::messages::write_bool),
        PrimitiveType::Char => quote!(blueberry::messages::write_char),
        PrimitiveType::Octet => quote!(blueberry::messages::write_u8),
        PrimitiveType::I16 => quote!(blueberry::messages::write_i16),
        PrimitiveType::U16 => quote!(blueberry::messages::write_u16_val),
        PrimitiveType::I32 => quote!(blueberry::messages::write_i32),
        PrimitiveType::U32 => quote!(blueberry::messages::write_u32_val),
        PrimitiveType::I64 => quote!(blueberry::messages::write_i64),
        PrimitiveType::U64 => quote!(blueberry::messages::write_u64_val),
        PrimitiveType::F32 => quote!(blueberry::messages::write_f32),
        PrimitiveType::F64 => quote!(blueberry::messages::write_f64),
    }
}

fn cpp_reader(primitive: PrimitiveType) -> TokenStream {
    match primitive {
        PrimitiveType::Bool => quote!(blueberry::messages::read_bool),
        PrimitiveType::Char => quote!(blueberry::messages::read_char),
        PrimitiveType::Octet => quote!(blueberry::messages::read_u8),
        PrimitiveType::I16 => quote!(blueberry::messages::read_i16),
        PrimitiveType::U16 => quote!(blueberry::messages::read_u16),
        PrimitiveType::I32 => quote!(blueberry::messages::read_i32),
        PrimitiveType::U32 => quote!(blueberry::messages::read_u32),
        PrimitiveType::I64 => quote!(blueberry::messages::read_i64),
        PrimitiveType::U64 => quote!(blueberry::messages::read_u64),
        PrimitiveType::F32 => quote!(blueberry::messages::read_f32),
        PrimitiveType::F64 => quote!(blueberry::messages::read_f64),
    }
}

const CPP_HELPERS: &str = "inline void write_u16(std::uint16_t value, std::uint8_t *out) {
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
";

const MESSAGE_HEADER: &str = "struct MessageHeader {
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
      throw std::runtime_error(\"frame shorter than header\");
    }
    MessageHeader header{};
    header.payload_words = read_u16(frame.data());
    header.flags = read_u16(frame.data() + 2);
    header.module_key = read_u16(frame.data() + 4);
    header.message_key = read_u16(frame.data() + 6);
    return header;
  }
};
";
