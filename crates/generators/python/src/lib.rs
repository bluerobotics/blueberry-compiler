use blueberry_ast::{
    Annotation, AnnotationParam, Commented, ConstDef, ConstValue, Definition, EnumDef, MessageDef,
    StructDef, Type, TypeDef,
};
use blueberry_codegen_core::{CodegenError, GeneratedFile, TypeRegistry};
use genco::lang::python::Tokens;
use genco::quote;

const OUTPUT_PATH: &str = "python/messages.py";

pub fn generate(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let generator = PythonGenerator::new(definitions);
    let contents = generator.render(definitions)?;
    Ok(vec![GeneratedFile {
        path: OUTPUT_PATH.to_string(),
        contents,
    }])
}

struct PythonGenerator {
    registry: TypeRegistry,
}

impl PythonGenerator {
    fn new(definitions: &[Definition]) -> Self {
        Self {
            registry: TypeRegistry::new(definitions),
        }
    }

    fn render(&self, definitions: &[Definition]) -> Result<String, CodegenError> {
        let defs = self.emit_definitions(definitions, &mut Vec::new())?;
        let helpers = helpers_tokens();
        let tokens: Tokens = quote! {
            # Auto-generated Blueberry bindings

            from __future__ import annotations
            from dataclasses import dataclass
            import enum
            import struct
            from typing import ClassVar, Callable, List

            $helpers

            $(for def in defs =>
                $def

            )
        };

        Ok(tokens.to_file_string().expect("render python output"))
    }

    fn emit_definitions(
        &self,
        defs: &[Definition],
        scope: &mut Vec<String>,
    ) -> Result<Vec<Tokens>, CodegenError> {
        let mut out = Vec::new();
        let mut message_keys = MessageKeyGen::default();

        for def in defs {
            match def {
                Definition::ModuleDef(module) => {
                    scope.push(module.node.name.clone());
                    let nested = self.emit_definitions(&module.node.definitions, scope)?;
                    out.extend(nested);
                    scope.pop();
                }
                Definition::EnumDef(enum_def) => {
                    out.push(quote! { $['\n'] });
                    out.push(self.emit_enum(enum_def, scope));
                }
                Definition::StructDef(struct_def) => {
                    out.push(quote! { $['\n'] });
                    out.push(self.emit_struct(struct_def, scope));
                }
                Definition::MessageDef(message_def) => {
                    let module_key =
                        annotation_u16(&message_def.annotations, "module_key").unwrap_or(0x4242);
                    let message_key = annotation_u16(&message_def.annotations, "message_key")
                        .unwrap_or_else(|| message_keys.next());
                    out.push(quote! { $['\n'] });
                    out.push(self.emit_message(message_def, scope, module_key, message_key)?);
                }
                Definition::ConstDef(const_def) => {
                    out.push(quote! { $['\r'] });
                    out.push(self.emit_const(const_def, scope));
                }
                Definition::TypeDef(typedef_def) => {
                    out.push(quote! { $['\r'] });
                    out.push(self.emit_typedef(typedef_def, scope));
                }
                Definition::ImportDef(_) => {}
            }
        }

        Ok(out)
    }

    fn emit_enum(&self, enum_def: &Commented<EnumDef>, scope: &[String]) -> Tokens {
        let name = class_name(scope, &enum_def.node.name);
        let members: Vec<Tokens> = enum_def
            .node
            .enumerators
            .iter()
            .enumerate()
            .map(|(idx, member)| {
                let value = member
                    .value
                    .as_ref()
                    .and_then(|v| match v {
                        ConstValue::Integer(lit) => Some(lit.value),
                        ConstValue::Binary(bin) => Some(bin.to_i128()),
                        _ => None,
                    })
                    .unwrap_or(idx as i128);
                quote!( $(member.name.clone()) = $(value) )
            })
            .collect();

        quote! {
            class $name(enum.IntEnum):
                $(for m in members =>
                    $m
                )
        }
    }

    fn emit_struct(&self, struct_def: &Commented<StructDef>, scope: &[String]) -> Tokens {
        let mut path = scope.to_vec();
        path.push(struct_def.node.name.clone());
        let members = self.registry.collect_struct_members(&path);
        let name = class_name(scope, &struct_def.node.name);
        let name_ref = name.as_str();
        let fields: Tokens = quote! {
            $(for member in &members =>
                $(member.name.clone()): $(python_type_hint(&member.ty, scope, &self.registry))$['\r']
            )
        };
        let serialize_fields: Vec<Tokens> = members
            .iter()
            .map(|member| self.serialize_value(&member.name, &member.ty, scope))
            .collect();
        let deserialize_fields: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let var_name = format!("value_{}", member.name);
                let read_expr = self.deserialize_value(&member.ty, scope);
                quote!( $var_name = $read_expr )
            })
            .collect();
        let assignments: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let var_name = format!("value_{}", member.name);
                let field = &member.name;
                quote!( $field=$var_name )
            })
            .collect();

        quote! {
            @dataclass
            class $name_ref:
                $fields

                def to_payload(self) -> bytes:
                    writer = CdrWriter()
                    self.serialize(writer)
                    return bytes(writer.buffer)

                def serialize(self, writer: "CdrWriter") -> None:
                    $(for s in &serialize_fields =>$s$['\r'])

                @classmethod
                def from_payload(cls, payload: bytes) -> $name_ref:
                    reader = CdrReader(payload)
                    return cls.read(reader)

                @classmethod
                def read(cls, reader: "CdrReader") -> $name_ref:
                    $(for d in &deserialize_fields => $d$['\r'])
                    return cls($(for a in &assignments join (, ) => $a))
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
        let name = class_name(scope, &message_def.node.name);
        let name_ref = name.as_str();
        let fields: Tokens = quote! {
            $(for member in &members =>
                $(member.name.clone()): $(python_type_hint(&member.ty, scope, &self.registry))$['\r']
            )
        };
        let serialize_fields: Vec<Tokens> = members
            .iter()
            .map(|member| self.serialize_value(&member.name, &member.ty, scope))
            .collect();
        let deserialize_fields: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let var_name = format!("value_{}", member.name);
                let read_expr = self.deserialize_value(&member.ty, scope);
                quote!( $var_name = $read_expr )
            })
            .collect();
        let assignments: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let var_name = format!("value_{}", member.name);
                let field = &member.name;
                quote!( $field=$var_name )
            })
            .collect();
        let topic_literal = quoted_string(&topic);

        Ok(quote! {
            @dataclass
            class $name_ref:
                topic_template: ClassVar[str] = $topic_literal
                module_key: ClassVar[int] = $module_key
                message_key: ClassVar[int] = $message_key

                $fields

                def to_payload(self) -> bytes:
                    writer = CdrWriter()
                    self.serialize(writer)
                    return bytes(writer.buffer)

                def serialize(self, writer: "CdrWriter") -> None:
                    $(for s in &serialize_fields => $s$['\r'])

                @classmethod
                def from_payload(cls, payload: bytes) -> $name_ref:
                    reader = CdrReader(payload)
                    return cls.read(reader)

                @classmethod
                def read(cls, reader: "CdrReader") -> $name_ref:
                    $(for d in &deserialize_fields => $d$['\r'])
                    return cls($(for a in &assignments join (, ) => $a))

                @classmethod
                def from_frame(cls, frame: bytes) -> $name_ref:
                    header = MessageHeader.parse(frame)
                    payload_len = header.payload_words * 4
                    payload = frame[MessageHeader.HEADER_LEN:MessageHeader.HEADER_LEN + payload_len]
                    return cls.from_payload(payload)

                @classmethod
                def topic(cls, device_type: str, nid: str) -> str:
                    return cls.topic_template.format(device_type=device_type, nid=nid)

                @staticmethod
                def subscribe(session, device_type: str, nid: str, callback) -> None:
                    topic = $name_ref.topic(device_type, nid)
                    session.subscribe(topic, lambda frame: callback($name_ref.from_frame(frame)))

                def publish(self, session, device_type: str, nid: str) -> None:
                    topic = self.topic(device_type, nid)
                    payload = self.to_payload()
                    header = MessageHeader(CdrWriter.payload_words_for(payload), 0, self.module_key, self.message_key)
                    session.put(topic, header.pack() + payload)

                @staticmethod
                def payload_words(payload: bytes) -> int:
                    return CdrWriter.payload_words_for(payload)
        })
    }

    fn emit_const(&self, const_def: &Commented<ConstDef>, scope: &[String]) -> Tokens {
        let name = class_name(scope, &const_def.node.name);
        let value = const_literal(&const_def.node.value);
        quote!( $name = $value )
    }

    fn emit_typedef(&self, typedef_def: &Commented<TypeDef>, scope: &[String]) -> Tokens {
        let name = class_name(scope, &typedef_def.node.name);
        let base = python_type_hint(&typedef_def.node.base_type, scope, &self.registry);
        quote!( $name = $base )
    }

    fn serialize_value(&self, name: &str, ty: &Type, scope: &[String]) -> Tokens {
        match self.registry.resolve_type(ty, scope) {
            Type::Boolean => quote!( writer.write_bool(self.$name) ),
            Type::Char => quote!( writer.write_char(self.$name) ),
            Type::Octet => quote!( writer.write_u8(self.$name) ),
            Type::Short => quote!( writer.write_i16(self.$name) ),
            Type::UnsignedShort => quote!( writer.write_u16(self.$name) ),
            Type::Long => quote!( writer.write_i32(self.$name) ),
            Type::UnsignedLong => quote!( writer.write_u32(self.$name) ),
            Type::LongLong => quote!( writer.write_i64(self.$name) ),
            Type::UnsignedLongLong => quote!( writer.write_u64(self.$name) ),
            Type::Float => quote!( writer.write_f32(self.$name) ),
            Type::Double => quote!( writer.write_f64(self.$name) ),
            Type::String { .. } => quote!( writer.write_string(self.$name) ),
            Type::Sequence { element_type, .. } => {
                let inner = self.serialize_value("item", &element_type, scope);
                quote! {
                    writer.write_sequence(self.$name, lambda w, item: ($inner))
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
                self.serialize_value(name, &elem, scope)
            }
            Type::ScopedName(path) => {
                if let Some(base) = self.registry.enum_base(&path) {
                    let write_fn = writer_for(base);
                    quote!( writer.$write_fn(int(self.$name)) )
                } else {
                    quote!( self.$name.serialize(writer) )
                }
            }
            Type::WString | Type::WChar | Type::LongDouble => quote!(None),
        }
    }

    fn deserialize_value(&self, ty: &Type, scope: &[String]) -> Tokens {
        match self.registry.resolve_type(ty, scope) {
            Type::Boolean => quote!(reader.read_bool()),
            Type::Char => quote!(reader.read_char()),
            Type::Octet => quote!(reader.read_u8()),
            Type::Short => quote!(reader.read_i16()),
            Type::UnsignedShort => quote!(reader.read_u16()),
            Type::Long => quote!(reader.read_i32()),
            Type::UnsignedLong => quote!(reader.read_u32()),
            Type::LongLong => quote!(reader.read_i64()),
            Type::UnsignedLongLong => quote!(reader.read_u64()),
            Type::Float => quote!(reader.read_f32()),
            Type::Double => quote!(reader.read_f64()),
            Type::String { .. } => quote!(reader.read_string()),
            Type::Sequence { element_type, .. } => {
                let inner = self.deserialize_value(&element_type, scope);
                quote! {
                    reader.read_sequence(lambda r: ($inner))
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
                self.deserialize_value(&elem, scope)
            }
            Type::ScopedName(path) => {
                if let Some(base) = self.registry.enum_base(&path) {
                    let read_fn = reader_for(base);
                    let scoped = class_name_path(&path);
                    quote!( $scoped(reader.$read_fn()) )
                } else {
                    let scoped = class_name_path(&path);
                    quote!( $scoped.read(reader) )
                }
            }
            Type::WString | Type::WChar | Type::LongDouble => quote!(None),
        }
    }
}

fn helpers_tokens() -> Tokens {
    quote! {
        class CdrWriter:
            def __init__(self) -> None:
                self.buffer: bytearray = bytearray()

            @staticmethod
            def payload_words_for(payload: bytes) -> int:
                return (len(payload) + 3) // 4

            def align(self, alignment: int) -> None:
                if alignment <= 1:
                    return
                pad = (alignment - (len(self.buffer) % alignment)) % alignment
                if pad:
                    self.buffer.extend(b"\x00" * pad)

            def write_bool(self, value: bool) -> None:
                self.align(1)
                self.buffer.append(1 if value else 0)

            def write_char(self, value: str) -> None:
                self.align(1)
                b = value.encode("latin1") if value else b"\x00"
                self.buffer.extend(b[:1].ljust(1, b"\x00"))

            def write_u8(self, value: int) -> None:
                self.align(1)
                self.buffer.extend(struct.pack("<B", value))

            def write_i16(self, value: int) -> None:
                self.align(2)
                self.buffer.extend(struct.pack("<h", value))

            def write_u16(self, value: int) -> None:
                self.align(2)
                self.buffer.extend(struct.pack("<H", value))

            def write_i32(self, value: int) -> None:
                self.align(4)
                self.buffer.extend(struct.pack("<i", value))

            def write_u32(self, value: int) -> None:
                self.align(4)
                self.buffer.extend(struct.pack("<I", value))

            def write_i64(self, value: int) -> None:
                self.align(8)
                self.buffer.extend(struct.pack("<q", value))

            def write_u64(self, value: int) -> None:
                self.align(8)
                self.buffer.extend(struct.pack("<Q", value))

            def write_f32(self, value: float) -> None:
                self.align(4)
                self.buffer.extend(struct.pack("<f", value))

            def write_f64(self, value: float) -> None:
                self.align(8)
                self.buffer.extend(struct.pack("<d", value))

            def write_string(self, value: str) -> None:
                self.align(4)
                data = value.encode("utf-8") + b"\x00"
                self.write_u32(len(data))
                self.buffer.extend(data)

            def write_sequence(self, values, write_fn: Callable[["CdrWriter", object], None]) -> None:
                self.align(4)
                self.write_u32(len(values))
                for item in values:
                    write_fn(self, item)

        class CdrReader:
            def __init__(self, data: bytes) -> None:
                self.data = data
                self.offset = 0

            def align(self, alignment: int) -> None:
                if alignment <= 1:
                    return
                pad = (alignment - (self.offset % alignment)) % alignment
                if self.offset + pad > len(self.data):
                    raise RuntimeError("cursor out of range")
                self.offset += pad

            def take(self, size: int) -> bytes:
                if self.offset + size > len(self.data):
                    raise RuntimeError("cursor out of range")
                start = self.offset
                self.offset += size
                return self.data[start:self.offset]

            def read_bool(self) -> bool:
                self.align(1)
                return bool(self.take(1)[0])

            def read_char(self) -> str:
                self.align(1)
                return self.take(1).decode("latin1")

            def read_u8(self) -> int:
                self.align(1)
                return struct.unpack("<B", self.take(1))[0]

            def read_i16(self) -> int:
                self.align(2)
                return struct.unpack("<h", self.take(2))[0]

            def read_u16(self) -> int:
                self.align(2)
                return struct.unpack("<H", self.take(2))[0]

            def read_i32(self) -> int:
                self.align(4)
                return struct.unpack("<i", self.take(4))[0]

            def read_u32(self) -> int:
                self.align(4)
                return struct.unpack("<I", self.take(4))[0]

            def read_i64(self) -> int:
                self.align(8)
                return struct.unpack("<q", self.take(8))[0]

            def read_u64(self) -> int:
                self.align(8)
                return struct.unpack("<Q", self.take(8))[0]

            def read_f32(self) -> float:
                self.align(4)
                return struct.unpack("<f", self.take(4))[0]

            def read_f64(self) -> float:
                self.align(8)
                return struct.unpack("<d", self.take(8))[0]

            def read_string(self) -> str:
                self.align(4)
                length = self.read_u32()
                if length == 0:
                    return ""
                data = self.take(length)
                if data[-1] != 0:
                    raise RuntimeError("CDR string missing null terminator")
                return data[:-1].decode("utf-8")

            def read_sequence(self, read_fn: Callable[["CdrReader"], object]):
                self.align(4)
                length = self.read_u32()
                values = []
                for _ in range(length):
                    values.append(read_fn(self))
                return values

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

fn writer_for(ty: &Type) -> Tokens {
    match ty {
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
    }
}

fn reader_for(ty: &Type) -> Tokens {
    match ty {
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
                        Some(value)
                    }
                    AnnotationParam::Positional(value) => Some(value),
                    AnnotationParam::Named { value, .. } => Some(value),
                })
                .next()
                .flatten()
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
        ConstValue::Integer(lit) if (0..=u16::MAX as i128).contains(&lit.value) => {
            Some(lit.value as u16)
        }
        _ => None,
    })
}

fn class_name(scope: &[String], name: &str) -> String {
    let mut parts = scope.to_vec();
    parts.push(name.to_string());
    parts.join("_")
}

fn class_name_path(path: &[String]) -> String {
    path.join("_")
}

fn scoped_name(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
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

fn python_type_hint(ty: &Type, scope: &[String], registry: &TypeRegistry) -> Tokens {
    match registry.resolve_type(ty, scope) {
        Type::Boolean => quote!(bool),
        Type::Char => quote!(str),
        Type::Octet
        | Type::Short
        | Type::UnsignedShort
        | Type::Long
        | Type::UnsignedLong
        | Type::LongLong
        | Type::UnsignedLongLong => quote!(int),
        Type::Float | Type::Double => quote!(float),
        Type::String { .. } => quote!(str),
        Type::Sequence { element_type, .. } => {
            let inner = python_type_hint(&element_type, scope, registry);
            quote!(List[$inner])
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
            python_type_hint(&elem, scope, registry)
        }
        Type::ScopedName(path) => {
            let ident = class_name_path(&path);
            quote!( $ident )
        }
        Type::WString | Type::WChar | Type::LongDouble => quote!(object),
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
            let value = bin.to_i128();
            quote!( $value )
        }
        ConstValue::String(s) => quote!( $(quoted_string(s)) ),
        ConstValue::Boolean(value) => {
            if *value {
                quote!(True)
            } else {
                quote!(False)
            }
        }
        ConstValue::Char(ch) => quote!( $(format!("'{}'", ch)) ),
        ConstValue::ScopedName(path) => quote!( $(path.join("::")) ),
        ConstValue::UnaryOp { .. } | ConstValue::BinaryOp { .. } => quote!(0),
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
