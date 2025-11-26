use blueberry_ast::Definition;
use blueberry_codegen_core::{
    CodegenError, GeneratedFile, MessageSpec, PrimitiveType, class_name, collect_messages,
    quoted_string,
};
use genco::lang::python::Tokens;
use genco::quote;

const OUTPUT_PATH: &str = "python/messages.py";

pub fn generate(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let messages = collect_messages(definitions)?;
    if messages.is_empty() {
        return Ok(Vec::new());
    }
    let generator = PythonGenerator::new(&messages);
    let contents = generator.render();
    Ok(vec![GeneratedFile {
        path: OUTPUT_PATH.to_string(),
        contents,
    }])
}

struct PythonGenerator<'a> {
    messages: &'a [MessageSpec],
}

impl<'a> PythonGenerator<'a> {
    fn new(messages: &'a [MessageSpec]) -> Self {
        Self { messages }
    }

    fn render(&self) -> String {
        let classes: Vec<Tokens> = self.messages.iter().map(|m| self.emit_message(m)).collect();
        let header = message_header();
        let tokens: Tokens = quote! {
            # Auto-generated Blueberry bindings

            from __future__ import annotations
            from dataclasses import dataclass
            import struct
            from typing import ClassVar

            $header

            $(for class_def in &classes =>
                $class_def

            )
        };

        tokens.to_file_string().expect("render python output")
    }

    fn emit_message(&self, message: &MessageSpec) -> Tokens {
        let class_ident = class_name(&message.scope, &message.name);
        let topic_literal = quoted_string(&message.topic);
        let struct_format = quoted_string(&format!(
            "<{}",
            message
                .fields
                .iter()
                .map(|field| python_struct_code(field.primitive))
                .collect::<String>()
        ));
        let module_key = message.module_key;
        let message_key = message.message_key;
        let struct_size = message.field_payload_size();
        let padded_size = message.padded_payload_size();
        let payload_words = message.payload_words();
        let field_defs: Tokens = quote! {
            $(for field in &message.fields =>
                $(field.name.clone()): $(python_type_hint(field.primitive))$['\n']
            )
        };

        let payload_methods = self.payload_tokens(message, &class_ident);
        let frame_helpers = frame_helper_tokens(&class_ident);
        let publish_helpers = publish_helper_tokens(&class_ident);

        quote! {
            @dataclass
            class $class_ident:
                topic_template: ClassVar[str] = $topic_literal
                module_key: ClassVar[int] = $module_key
                message_key: ClassVar[int] = $message_key
                STRUCT_FORMAT: ClassVar[str] = $struct_format
                STRUCT_SIZE: ClassVar[int] = $struct_size
                PADDED_SIZE: ClassVar[int] = $padded_size
                PAYLOAD_WORDS: ClassVar[int] = $payload_words

                $field_defs

                $payload_methods

                $frame_helpers

                $publish_helpers
        }
    }

    fn payload_tokens(&self, message: &MessageSpec, class_ident: &str) -> Tokens {
        if message.fields.is_empty() {
            return quote! {
                def to_payload(self) -> bytes:
                    return b"".ljust(self.PADDED_SIZE, b"\x00")

                @classmethod
                def from_payload(cls, payload: bytes) -> $class_ident:
                    return cls()
            };
        }

        let pack_args: Tokens =
            quote! { $(for field in &message.fields join (, ) => self.$(field.name.clone())) };
        let value_inits: Tokens = quote! {
            $(for (idx, field) in message.fields.iter().enumerate() join (, ) =>
                $(field.name.clone()) = values[$idx]
            )
        };

        quote! {
            def to_payload(self) -> bytes:
                payload = struct.pack(self.STRUCT_FORMAT, $pack_args)
                if len(payload) < self.PADDED_SIZE:
                    payload += b"\x00" * (self.PADDED_SIZE - len(payload))
                return payload

            @classmethod
            def from_payload(cls, payload: bytes) -> $class_ident:
                values = struct.unpack(cls.STRUCT_FORMAT, payload[: cls.STRUCT_SIZE])
                return cls($value_inits)
        }
    }
}

fn frame_helper_tokens(class_ident: &str) -> Tokens {
    quote! {
        @classmethod
        def from_frame(cls, frame: bytes) -> $class_ident:
            header = MessageHeader.parse(frame)
            payload_len = header.payload_words * 4
            payload = frame[MessageHeader.HEADER_LEN:MessageHeader.HEADER_LEN + payload_len]
            return cls.from_payload(payload)
    }
}

fn publish_helper_tokens(class_ident: &str) -> Tokens {
    quote! {
        @staticmethod
        def subscribe(session, device_type: str, nid: str, callback) -> None:
            topic = $class_ident.topic_template.format(device_type=device_type, nid=nid)
            session.subscribe(topic, lambda frame: callback($class_ident.from_frame(frame)))

        def publish(self, session, device_type: str, nid: str) -> None:
            topic = self.topic_template.format(device_type=device_type, nid=nid)
            payload = self.to_payload()
            header = MessageHeader(self.PAYLOAD_WORDS, 0, self.module_key, self.message_key)
            session.put(topic, header.pack() + payload)
    }
}

fn message_header() -> Tokens {
    quote! {
        @dataclass
        class MessageHeader:
            payload_words: int
            flags: int
            module_key: int
            message_key: int

            HEADER_LEN: ClassVar[int] = 8

            def pack(self) -> bytes:
                return struct.pack("<HHHH", self.payload_words, self.flags, self.module_key, self.message_key)

            @classmethod
            def parse(cls, raw: bytes) -> "MessageHeader":
                payload_words, flags, module_key, message_key = struct.unpack("<HHHH", raw[: cls.HEADER_LEN])
                return cls(payload_words, flags, module_key, message_key)
    }
}

fn python_type_hint(primitive: PrimitiveType) -> Tokens {
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
