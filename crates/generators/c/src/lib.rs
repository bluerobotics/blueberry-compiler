use blueberry_ast::Definition;
use blueberry_codegen_core::{
    CodegenError, GeneratedFile, MESSAGE_HEADER_SIZE, MessageSpec, PrimitiveType, SourceWriter,
    TopicPlaceholder, collect_messages, snake_case_path, topic_format, uppercase_path,
};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

const OUTPUT_PATH: &str = "c/messages.h";
const TOPIC_BUFFER_LEN: usize = 256;

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
    writer.push_str("#include <stdbool.h>\n");
    writer.push_str("#include <stddef.h>\n");
    writer.push_str("#include <stdint.h>\n");
    writer.push_str("#include <stdio.h>\n");
    writer.push_str("#include <string.h>\n\n");
    writer.push_str(&format!(
        "#define BLUEBERRY_MESSAGE_HEADER_SIZE {MESSAGE_HEADER_SIZE}u\n#define BLUEBERRY_MAX_TOPIC_LENGTH {TOPIC_BUFFER_LEN}u\n\n"
    ));
    writer.push_str(
        "typedef struct {\n    uint16_t payload_words;\n    uint16_t flags;\n    uint16_t module_key;\n    uint16_t message_key;\n} blueberry_message_header_t;\n\n",
    );
    writer.push_str(
        "typedef int (*blueberry_publish_fn)(const char *topic, const uint8_t *frame, size_t frame_len, void *user_data);\n",
    );
    writer.push_str(COMMON_HELPERS);
    for message in messages {
        writer.newline();
        render_message(&mut writer, message);
    }
    writer.into_string()
}

fn render_message(writer: &mut SourceWriter, message: &MessageSpec) {
    let snake_name = snake_case_path(&message.scope, &message.name);
    let struct_ident = format_ident!("blueberry_{}_t", snake_name);
    writer.push_str("typedef struct {\n");
    for field in &message.fields {
        let field_type = c_field_type(field.primitive);
        let field_ident = format_ident!("{}", field.name);
        indent(writer, 1, quote! { #field_type #field_ident; });
    }
    writer.push_str("} ");
    writer.push_line(quote! { #struct_ident; });
    writer.newline();

    let upper = uppercase_path(&message.scope, &message.name);
    let struct_macro = format_ident!("{}_STRUCT_SIZE", upper);
    let padded_macro = format_ident!("{}_PADDED_SIZE", upper);
    let payload_macro = format_ident!("{}_PAYLOAD_WORDS", upper);
    let module_macro = format_ident!("{}_MODULE_KEY", upper);
    let message_macro = format_ident!("{}_MESSAGE_KEY", upper);
    let struct_size = message.field_payload_size();
    let padded_size = message.padded_payload_size();
    let payload_words = message.payload_words();
    writer.push_str(&format!(
        "#define {upper}_STRUCT_SIZE {struct_size}u\n#define {upper}_PADDED_SIZE {padded_size}u\n#define {upper}_PAYLOAD_WORDS {payload_words}u\n#define {upper}_MODULE_KEY 0x{module:04X}u\n#define {upper}_MESSAGE_KEY 0x{message_key:04X}u\n",
        upper = upper,
        struct_size = struct_size,
        padded_size = padded_size,
        payload_words = payload_words,
        module = message.module_key,
        message_key = message.message_key,
    ));
    let topic = topic_format(&message.topic);
    let topic_literal = Literal::string(&topic.template);
    writer.push_str(&format!(
        "#define {upper}_TOPIC_FMT {literal}\n",
        upper = upper,
        literal = topic_literal,
    ));
    writer.newline();

    let pack_fn = format_ident!("{}_pack", snake_name);
    start_block(
        writer,
        0,
        quote! { static inline void #pack_fn(const #struct_ident *msg, uint8_t *payload) },
    );
    indent(writer, 1, quote! { memset(payload, 0, #padded_macro); });
    indent(writer, 1, quote! { size_t offset = 0; });
    for field in &message.fields {
        let writer_fn = c_writer(field.primitive);
        let field_ident = format_ident!("{}", field.name);
        let field_size = Literal::usize_unsuffixed(field.primitive.size());
        indent(
            writer,
            1,
            quote! { #writer_fn(msg->#field_ident, payload + offset); },
        );
        indent(writer, 1, quote! { offset += #field_size; });
    }
    close_block(writer, 0);
    writer.newline();

    let parse_fn = format_ident!("{}_parse", snake_name);
    start_block(
        writer,
        0,
        quote! { static inline int #parse_fn(const uint8_t *payload, size_t payload_len, #struct_ident *out) },
    );
    indent(
        writer,
        1,
        quote! { if (payload_len < #struct_macro) { return -1; } },
    );
    indent(writer, 1, quote! { size_t offset = 0; });
    for field in &message.fields {
        let reader_fn = c_reader(field.primitive);
        let field_ident = format_ident!("{}", field.name);
        let field_size = Literal::usize_unsuffixed(field.primitive.size());
        indent(
            writer,
            1,
            quote! { out->#field_ident = #reader_fn(payload + offset); },
        );
        indent(writer, 1, quote! { offset += #field_size; });
    }
    indent(writer, 1, quote! { return 0; });
    close_block(writer, 0);
    writer.newline();

    let from_frame_fn = format_ident!("{}_from_frame", snake_name);
    start_block(
        writer,
        0,
        quote! { static inline int #from_frame_fn(const uint8_t *frame, size_t frame_len, #struct_ident *out) },
    );
    indent(writer, 1, quote! { blueberry_message_header_t header; });
    indent(
        writer,
        1,
        quote! { if (blueberry_read_header(frame, frame_len, &header) != 0) { return -1; } },
    );
    indent(
        writer,
        1,
        quote! { if (header.module_key != #module_macro || header.message_key != #message_macro) { return -2; } },
    );
    indent(
        writer,
        1,
        quote! { size_t payload_len = (size_t)header.payload_words * 4u; },
    );
    indent(
        writer,
        1,
        quote! { if (frame_len < BLUEBERRY_MESSAGE_HEADER_SIZE + payload_len) { return -3; } },
    );
    indent(
        writer,
        1,
        quote! { return #parse_fn(frame + BLUEBERRY_MESSAGE_HEADER_SIZE, payload_len, out); },
    );
    close_block(writer, 0);
    writer.newline();

    let callback_ident = format_ident!("{}_callback", snake_name);
    writer.push_line(
        quote! { typedef void (*#callback_ident)(const #struct_ident *msg, void *user_data); },
    );
    writer.newline();
    let dispatch_fn = format_ident!("{}_dispatch", snake_name);
    start_block(
        writer,
        0,
        quote! { static inline void #dispatch_fn(const uint8_t *frame, size_t frame_len, #callback_ident callback, void *user_data) },
    );
    indent(writer, 1, quote! { #struct_ident msg; });
    indent(
        writer,
        1,
        quote! { if (#from_frame_fn(frame, frame_len, &msg) == 0) { callback(&msg, user_data); } },
    );
    close_block(writer, 0);
    writer.newline();

    let publish_fn = format_ident!("{}_publish", snake_name);
    start_block(
        writer,
        0,
        quote! { static inline int #publish_fn(blueberry_publish_fn publish, void *user_data, const char *device_type, const char *nid, const #struct_ident *msg) },
    );
    indent(
        writer,
        1,
        quote! { uint8_t frame[BLUEBERRY_MESSAGE_HEADER_SIZE + #padded_macro] = {0}; },
    );
    start_initializer(writer, 1, quote! { blueberry_message_header_t header });
    indent(writer, 2, quote! { .payload_words = #payload_macro, });
    indent(writer, 2, quote! { .flags = 0, });
    indent(writer, 2, quote! { .module_key = #module_macro, });
    indent(writer, 2, quote! { .message_key = #message_macro, });
    end_initializer(writer, 1);
    indent(
        writer,
        1,
        quote! { blueberry_write_header(&header, frame); },
    );
    indent(
        writer,
        1,
        quote! { #pack_fn(msg, frame + BLUEBERRY_MESSAGE_HEADER_SIZE); },
    );
    indent(
        writer,
        1,
        quote! { char topic[BLUEBERRY_MAX_TOPIC_LENGTH]; },
    );
    let fmt_literal = Literal::string(&topic.template);
    let fmt_args = c_format_arguments(&topic.placeholders);
    indent(
        writer,
        1,
        quote! { int topic_len = snprintf(topic, sizeof(topic), #fmt_literal #(, #fmt_args)*); },
    );
    indent(
        writer,
        1,
        quote! { if (topic_len < 0 || (size_t)topic_len >= sizeof(topic)) { return -4; } },
    );
    indent(
        writer,
        1,
        quote! { return publish(topic, frame, sizeof(frame), user_data); },
    );
    close_block(writer, 0);
    writer.newline();
}

fn indent(writer: &mut SourceWriter, level: usize, tokens: TokenStream) {
    for _ in 0..level {
        writer.push_str("    ");
    }
    writer.push_line(tokens);
}

fn start_block(writer: &mut SourceWriter, level: usize, header: TokenStream) {
    for _ in 0..level {
        writer.push_str("    ");
    }
    writer.push(header);
    writer.push_str(" {\n");
}

fn close_block(writer: &mut SourceWriter, level: usize) {
    for _ in 0..level {
        writer.push_str("    ");
    }
    writer.push_str("}\n");
}

fn start_initializer(writer: &mut SourceWriter, level: usize, target: TokenStream) {
    for _ in 0..level {
        writer.push_str("    ");
    }
    writer.push(target);
    writer.push_str(" = {\n");
}

fn end_initializer(writer: &mut SourceWriter, level: usize) {
    for _ in 0..level {
        writer.push_str("    ");
    }
    writer.push_str("};\n");
}

fn c_field_type(primitive: PrimitiveType) -> TokenStream {
    match primitive {
        PrimitiveType::Bool => quote!(bool),
        PrimitiveType::Char => quote!(char),
        PrimitiveType::Octet => quote!(uint8_t),
        PrimitiveType::I16 => quote!(int16_t),
        PrimitiveType::U16 => quote!(uint16_t),
        PrimitiveType::I32 => quote!(int32_t),
        PrimitiveType::U32 => quote!(uint32_t),
        PrimitiveType::I64 => quote!(int64_t),
        PrimitiveType::U64 => quote!(uint64_t),
        PrimitiveType::F32 => quote!(float),
        PrimitiveType::F64 => quote!(double),
    }
}

fn c_reader(primitive: PrimitiveType) -> TokenStream {
    match primitive {
        PrimitiveType::Bool => quote!(blueberry_read_bool),
        PrimitiveType::Char => quote!(blueberry_read_char),
        PrimitiveType::Octet => quote!(blueberry_read_u8),
        PrimitiveType::I16 => quote!(blueberry_read_i16),
        PrimitiveType::U16 => quote!(blueberry_read_u16),
        PrimitiveType::I32 => quote!(blueberry_read_i32),
        PrimitiveType::U32 => quote!(blueberry_read_u32),
        PrimitiveType::I64 => quote!(blueberry_read_i64),
        PrimitiveType::U64 => quote!(blueberry_read_u64),
        PrimitiveType::F32 => quote!(blueberry_read_f32),
        PrimitiveType::F64 => quote!(blueberry_read_f64),
    }
}

fn c_writer(primitive: PrimitiveType) -> TokenStream {
    match primitive {
        PrimitiveType::Bool => quote!(blueberry_write_bool),
        PrimitiveType::Char => quote!(blueberry_write_char),
        PrimitiveType::Octet => quote!(blueberry_write_u8),
        PrimitiveType::I16 => quote!(blueberry_write_i16),
        PrimitiveType::U16 => quote!(blueberry_write_u16),
        PrimitiveType::I32 => quote!(blueberry_write_i32),
        PrimitiveType::U32 => quote!(blueberry_write_u32),
        PrimitiveType::I64 => quote!(blueberry_write_i64),
        PrimitiveType::U64 => quote!(blueberry_write_u64),
        PrimitiveType::F32 => quote!(blueberry_write_f32),
        PrimitiveType::F64 => quote!(blueberry_write_f64),
    }
}

fn c_format_arguments(placeholders: &[TopicPlaceholder]) -> Vec<TokenStream> {
    placeholders
        .iter()
        .map(|placeholder| match placeholder {
            TopicPlaceholder::DeviceType => quote!(device_type),
            TopicPlaceholder::Nid => quote!(nid),
        })
        .collect()
}

const COMMON_HELPERS: &str = "
static inline void blueberry_write_u16(uint16_t value, uint8_t *out) {
    out[0] = (uint8_t)(value & 0xffu);
    out[1] = (uint8_t)((value >> 8) & 0xffu);
}

static inline uint16_t blueberry_read_u16(const uint8_t *in) {
    return (uint16_t)in[0] | (uint16_t)(in[1] << 8);
}

static inline void blueberry_write_u32(uint32_t value, uint8_t *out) {
    for (int i = 0; i < 4; ++i) {
        out[i] = (uint8_t)((value >> (8 * i)) & 0xffu);
    }
}

static inline uint32_t blueberry_read_u32(const uint8_t *in) {
    return (uint32_t)in[0] | ((uint32_t)in[1] << 8) | ((uint32_t)in[2] << 16) | ((uint32_t)in[3] << 24);
}

static inline void blueberry_write_u64(uint64_t value, uint8_t *out) {
    for (int i = 0; i < 8; ++i) {
        out[i] = (uint8_t)((value >> (8 * i)) & 0xffu);
    }
}

static inline uint64_t blueberry_read_u64(const uint8_t *in) {
    uint64_t result = 0;
    for (int i = 0; i < 8; ++i) {
        result |= (uint64_t)in[i] << (8 * i);
    }
    return result;
}

static inline void blueberry_write_bool(bool value, uint8_t *out) {
    out[0] = value ? 1 : 0;
}

static inline bool blueberry_read_bool(const uint8_t *in) {
    return in[0] != 0;
}

static inline void blueberry_write_char(char value, uint8_t *out) {
    out[0] = (uint8_t)value;
}

static inline char blueberry_read_char(const uint8_t *in) {
    return (char)in[0];
}

static inline void blueberry_write_u8(uint8_t value, uint8_t *out) {
    out[0] = value;
}

static inline uint8_t blueberry_read_u8(const uint8_t *in) {
    return in[0];
}

static inline void blueberry_write_i16(int16_t value, uint8_t *out) {
    blueberry_write_u16((uint16_t)value, out);
}

static inline int16_t blueberry_read_i16(const uint8_t *in) {
    return (int16_t)blueberry_read_u16(in);
}

static inline void blueberry_write_u16_val(uint16_t value, uint8_t *out) {
    blueberry_write_u16(value, out);
}

static inline uint16_t blueberry_read_u16_val(const uint8_t *in) {
    return blueberry_read_u16(in);
}

static inline void blueberry_write_i32(int32_t value, uint8_t *out) {
    blueberry_write_u32((uint32_t)value, out);
}

static inline int32_t blueberry_read_i32(const uint8_t *in) {
    return (int32_t)blueberry_read_u32(in);
}

static inline void blueberry_write_u32_val(uint32_t value, uint8_t *out) {
    blueberry_write_u32(value, out);
}

static inline uint32_t blueberry_read_u32_val(const uint8_t *in) {
    return blueberry_read_u32(in);
}

static inline void blueberry_write_i64(int64_t value, uint8_t *out) {
    blueberry_write_u64((uint64_t)value, out);
}

static inline int64_t blueberry_read_i64(const uint8_t *in) {
    return (int64_t)blueberry_read_u64(in);
}

static inline void blueberry_write_u64_val(uint64_t value, uint8_t *out) {
    blueberry_write_u64(value, out);
}

static inline uint64_t blueberry_read_u64_val(const uint8_t *in) {
    return blueberry_read_u64(in);
}

static inline void blueberry_write_f32(float value, uint8_t *out) {
    uint32_t bits;
    memcpy(&bits, &value, sizeof(bits));
    blueberry_write_u32(bits, out);
}

static inline float blueberry_read_f32(const uint8_t *in) {
    uint32_t bits = blueberry_read_u32(in);
    float value;
    memcpy(&value, &bits, sizeof(value));
    return value;
}

static inline void blueberry_write_f64(double value, uint8_t *out) {
    uint64_t bits;
    memcpy(&bits, &value, sizeof(bits));
    blueberry_write_u64(bits, out);
}

static inline double blueberry_read_f64(const uint8_t *in) {
    uint64_t bits = blueberry_read_u64(in);
    double value;
    memcpy(&value, &bits, sizeof(value));
    return value;
}

static inline void blueberry_write_header(const blueberry_message_header_t *header, uint8_t *out) {
    blueberry_write_u16(header->payload_words, out);
    blueberry_write_u16(header->flags, out + 2);
    blueberry_write_u16(header->module_key, out + 4);
    blueberry_write_u16(header->message_key, out + 6);
}

static inline int blueberry_read_header(const uint8_t *frame, size_t frame_len, blueberry_message_header_t *out) {
    if (frame_len < BLUEBERRY_MESSAGE_HEADER_SIZE) {
        return -1;
    }
    out->payload_words = blueberry_read_u16(frame);
    out->flags = blueberry_read_u16(frame + 2);
    out->module_key = blueberry_read_u16(frame + 4);
    out->message_key = blueberry_read_u16(frame + 6);
    return 0;
}
";
