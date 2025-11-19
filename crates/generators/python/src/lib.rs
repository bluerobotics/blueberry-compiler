use blueberry_ast::Definition;
use blueberry_codegen_core::{
    CodegenError, GeneratedFile, MessageSpec, PrimitiveType, SourceWriter, class_name,
    collect_messages,
};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use std::str::FromStr;

const OUTPUT_PATH: &str = "python/messages.py";

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
    writer.push_str("# Auto-generated Blueberry bindings\n\n");
    writer.push_str("from dataclasses import dataclass\n");
    writer.push_str("import struct\n");
    writer.push_str("from typing import ClassVar\n\n");
    render_message_header(&mut writer);
    for message in messages {
        writer.newline();
        render_message(&mut writer, message);
    }
    writer.into_string()
}

fn render_message_header(writer: &mut SourceWriter) {
    writer.push_line(quote! { @dataclass });
    writer.push_line(quote! { class MessageHeader: });
    indent(writer, 1, quote! { payload_words: int });
    indent(writer, 1, quote! { flags: int });
    indent(writer, 1, quote! { module_key: int });
    indent(writer, 1, quote! { message_key: int });
    writer.newline();
    indent(writer, 1, quote! { HEADER_LEN: ClassVar[int] = 8 });
    writer.newline();
    indent(writer, 1, quote! { def pack(self) -> bytes: });
    indent(
        writer,
        2,
        quote! { return struct.pack("<HHHH", self.payload_words, self.flags, self.module_key, self.message_key) },
    );
    writer.newline();
    indent(writer, 1, quote! { @classmethod });
    indent(
        writer,
        1,
        quote! { def parse(cls, raw: bytes) -> "MessageHeader": },
    );
    indent(
        writer,
        2,
        quote! { payload_words, flags, module_key, message_key = struct.unpack("<HHHH", raw[: cls.HEADER_LEN]) },
    );
    indent(
        writer,
        2,
        quote! { return cls(payload_words, flags, module_key, message_key) },
    );
    writer.newline();
}

fn render_message(writer: &mut SourceWriter, message: &MessageSpec) {
    let class_ident = format_ident!("{}", class_name(&message.scope, &message.name));
    let class_literal = Literal::string(&class_ident.to_string());
    let struct_format: String = message
        .fields
        .iter()
        .map(|field| python_struct_code(field.primitive))
        .collect();
    let struct_format = Literal::string(&format!("<{struct_format}"));
    let topic_literal = Literal::string(&message.topic);
    let module_key = hex_literal(message.module_key);
    let message_key = hex_literal(message.message_key);
    let struct_size = message.field_payload_size();
    let padded_size = message.padded_payload_size();
    let payload_words = message.payload_words();
    writer.push_line(quote! { @dataclass });
    writer.push_line(quote! { class #class_ident: });
    indent(
        writer,
        1,
        quote! { topic_template: ClassVar[str] = #topic_literal },
    );
    indent(
        writer,
        1,
        quote! { module_key: ClassVar[int] = #module_key },
    );
    indent(
        writer,
        1,
        quote! { message_key: ClassVar[int] = #message_key },
    );
    indent(
        writer,
        1,
        quote! { STRUCT_FORMAT: ClassVar[str] = #struct_format },
    );
    indent(
        writer,
        1,
        quote! { STRUCT_SIZE: ClassVar[int] = #struct_size },
    );
    indent(
        writer,
        1,
        quote! { PADDED_SIZE: ClassVar[int] = #padded_size },
    );
    indent(
        writer,
        1,
        quote! { PAYLOAD_WORDS: ClassVar[int] = #payload_words },
    );
    writer.newline();
    for field in &message.fields {
        let field_ident = format_ident!("{}", field.name);
        let type_hint = python_type_hint(field.primitive);
        indent(writer, 1, quote! { #field_ident: #type_hint });
    }
    if message.fields.is_empty() {
        writer.newline();
        indent(writer, 1, quote! { def to_payload(self) -> bytes: });
        indent(
            writer,
            2,
            quote! { return b"".ljust(self.PADDED_SIZE, b"\x00") },
        );
        writer.newline();
        indent(writer, 1, quote! { @classmethod });
        indent(
            writer,
            1,
            quote! { def from_payload(cls, payload: bytes) -> #class_literal: },
        );
        indent(writer, 2, quote! { return cls() });
        writer.newline();
    } else {
        writer.newline();
        indent(writer, 1, quote! { def to_payload(self) -> bytes: });
        let pack_args: Vec<TokenStream> = message
            .fields
            .iter()
            .map(|field| {
                let field_ident = format_ident!("{}", field.name);
                quote!(self.#field_ident)
            })
            .collect();
        indent(
            writer,
            2,
            quote! { payload = struct.pack(self.STRUCT_FORMAT #(, #pack_args)*) },
        );
        indent(writer, 2, quote! { if len(payload) < self.PADDED_SIZE: });
        indent(
            writer,
            3,
            quote! { payload += b"\x00" * (self.PADDED_SIZE - len(payload)) },
        );
        indent(writer, 2, quote! { return payload });
        writer.newline();
        indent(writer, 1, quote! { @classmethod });
        indent(
            writer,
            1,
            quote! { def from_payload(cls, payload: bytes) -> #class_literal: },
        );
        indent(
            writer,
            2,
            quote! { values = struct.unpack(cls.STRUCT_FORMAT, payload[: cls.STRUCT_SIZE]) },
        );
        push_indented(writer, 2, "return cls(");
        for (idx, field) in message.fields.iter().enumerate() {
            let idx_lit = Literal::usize_unsuffixed(idx);
            let field_ident = format_ident!("{}", field.name);
            indent(writer, 3, quote! { #field_ident = values[#idx_lit], });
        }
        push_indented(writer, 2, ")");
        writer.newline();
    }
    indent(writer, 1, quote! { @classmethod });
    indent(
        writer,
        1,
        quote! { def from_frame(cls, frame: bytes) -> #class_literal: },
    );
    indent(writer, 2, quote! { header = MessageHeader.parse(frame) });
    indent(writer, 2, quote! { payload_len = header.payload_words * 4 });
    indent(
        writer,
        2,
        quote! { payload = frame[MessageHeader.HEADER_LEN:MessageHeader.HEADER_LEN + payload_len] },
    );
    indent(writer, 2, quote! { return cls.from_payload(payload) });
    writer.newline();
    indent(writer, 1, quote! { @staticmethod });
    indent(
        writer,
        1,
        quote! { def subscribe(session, device_type: str, nid: str, callback) -> None: },
    );
    indent(
        writer,
        2,
        quote! { topic = #class_ident.topic_template.format(device_type=device_type, nid=nid) },
    );
    indent(
        writer,
        2,
        quote! { session.subscribe(topic, lambda frame: callback(#class_ident.from_frame(frame))) },
    );
    writer.newline();
    indent(
        writer,
        1,
        quote! { def publish(self, session, device_type: str, nid: str) -> None: },
    );
    indent(
        writer,
        2,
        quote! { topic = self.topic_template.format(device_type=device_type, nid=nid) },
    );
    indent(writer, 2, quote! { payload = self.to_payload() });
    indent(
        writer,
        2,
        quote! { header = MessageHeader(self.PAYLOAD_WORDS, 0, self.module_key, self.message_key) },
    );
    indent(
        writer,
        2,
        quote! { session.put(topic, header.pack() + payload) },
    );
}

fn indent(writer: &mut SourceWriter, level: usize, tokens: TokenStream) {
    for _ in 0..level {
        writer.push_str("    ");
    }
    writer.push_line(tokens);
}

fn push_indented(writer: &mut SourceWriter, level: usize, text: &str) {
    for _ in 0..level {
        writer.push_str("    ");
    }
    writer.push_str(text);
    writer.push_str("\n");
}

fn python_type_hint(primitive: PrimitiveType) -> TokenStream {
    match primitive {
        PrimitiveType::Bool => quote!(bool),
        PrimitiveType::Char => quote!(str),
        PrimitiveType::Octet
        | PrimitiveType::I16
        | PrimitiveType::U16
        | PrimitiveType::I32
        | PrimitiveType::U32
        | PrimitiveType::I64
        | PrimitiveType::U64 => quote!(int),
        PrimitiveType::F32 | PrimitiveType::F64 => quote!(float),
    }
}

fn python_struct_code(primitive: PrimitiveType) -> &'static str {
    match primitive {
        PrimitiveType::Bool | PrimitiveType::Octet | PrimitiveType::Char => "B",
        PrimitiveType::I16 => "h",
        PrimitiveType::U16 => "H",
        PrimitiveType::I32 => "i",
        PrimitiveType::U32 => "I",
        PrimitiveType::I64 => "q",
        PrimitiveType::U64 => "Q",
        PrimitiveType::F32 => "f",
        PrimitiveType::F64 => "d",
    }
}

fn hex_literal(value: u16) -> TokenStream {
    let text = format!("0x{value:04x}");
    TokenStream::from_str(&text).expect("valid hex literal")
}
