use crate::ast::{
    Annotation, AnnotationParam, Commented, ConstDef, ConstValue, Definition, EnumDef,
    FixedPointLiteral, ImportDef, ImportScope, IntegerBase, IntegerLiteral, ModuleDef, StructDef,
    Type, TypeDef,
};
use proc_macro2::{Delimiter, Ident, Literal, Spacing, Span, TokenStream, TokenTree};
use quote::quote;
use std::{fmt, fs, path::Path, str::FromStr};

/// Serialize a slice of definitions back into an IDL source string.
pub fn generate_idl(definitions: &[Definition]) -> String {
    let mut generator = IdlGenerator::new();
    generator.emit_definitions(definitions, 0);
    generator.finish()
}

/// Write a slice of definitions to an IDL file on disk.
pub fn write_idl_file<P: AsRef<Path>>(definitions: &[Definition], path: P) -> std::io::Result<()> {
    let path = path.as_ref();
    if let Some(parent) = path.parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)?;
    }
    let contents = generate_idl(definitions);
    fs::write(path, contents)
}

struct IdlGenerator {
    buffer: String,
}

impl IdlGenerator {
    fn new() -> Self {
        Self {
            buffer: String::new(),
        }
    }

    fn finish(mut self) -> String {
        if !self.buffer.is_empty() && !self.buffer.ends_with('\n') {
            self.buffer.push('\n');
        }
        self.buffer
    }

    fn emit_definitions(&mut self, definitions: &[Definition], indent: usize) {
        for (idx, definition) in definitions.iter().enumerate() {
            if idx > 0 {
                self.buffer.push('\n');
            }
            self.emit_definition(definition, indent);
        }
    }

    fn emit_definition(&mut self, definition: &Definition, indent: usize) {
        match definition {
            Definition::ModuleDef(module) => self.emit_module(module, indent),
            Definition::TypeDef(typedef) => self.emit_typedef(typedef, indent),
            Definition::EnumDef(enum_def) => self.emit_enum(enum_def, indent),
            Definition::StructDef(struct_def) => self.emit_struct(struct_def, indent),
            Definition::ConstDef(const_def) => self.emit_const(const_def, indent),
            Definition::ImportDef(import_def) => self.emit_import(import_def, indent),
        }
    }

    fn emit_module(&mut self, module: &Commented<ModuleDef>, indent: usize) {
        self.emit_comments(&module.comments, indent);
        self.emit_annotations(&module.annotations, indent);
        let name = ident(&module.node.name);
        let header = quote!(module #name);
        self.write_tokens_with_suffix(indent, header, " {");
        self.emit_definitions(&module.node.definitions, indent + 1);
        self.write_raw_line(indent, "};");
    }

    fn emit_typedef(&mut self, typedef: &Commented<TypeDef>, indent: usize) {
        self.emit_comments(&typedef.comments, indent);
        self.emit_annotations(&typedef.annotations, indent);
        let name = ident(&typedef.node.name);
        let (base_type, dims) = split_array_type(&typedef.node.base_type);
        let base_tokens = render_type_tokens(base_type);
        let dims_tokens = render_array_dimensions_tokens(dims);
        let line = quote!(typedef #base_tokens #name #dims_tokens ;);
        self.write_tokens_line(indent, line);
    }

    fn emit_enum(&mut self, enum_def: &Commented<EnumDef>, indent: usize) {
        self.emit_comments(&enum_def.comments, indent);
        self.emit_annotations(&enum_def.annotations, indent);
        let name = ident(&enum_def.node.name);
        let header = if let Some(base_type) = &enum_def.node.base_type {
            let base_tokens = render_type_tokens(base_type);
            quote!(enum #name : #base_tokens)
        } else {
            quote!(enum #name)
        };
        self.write_tokens_with_suffix(indent, header, " {");
        for (idx, member) in enum_def.node.enumerators.iter().enumerate() {
            self.emit_comments(&member.comments, indent + 1);
            let member_name = ident(&member.name);
            let suffix = if idx + 1 < enum_def.node.enumerators.len() {
                ","
            } else {
                ""
            };
            if let Some(value) = &member.value {
                let value_fragment = render_const_value_fragment(value);
                let line = format_assignment(quote!(#member_name =), &value_fragment);
                self.write_raw_line_with_suffix(indent + 1, &line, suffix);
            } else {
                let tokens = quote!(#member_name);
                self.write_tokens_with_suffix(indent + 1, tokens, suffix);
            }
        }
        self.write_raw_line(indent, "};");
    }

    fn emit_struct(&mut self, struct_def: &Commented<StructDef>, indent: usize) {
        self.emit_comments(&struct_def.comments, indent);
        self.emit_annotations(&struct_def.annotations, indent);
        let name = ident(&struct_def.node.name);
        let header = quote!(struct #name);
        self.write_tokens_with_suffix(indent, header, " {");
        for member in &struct_def.node.members {
            self.emit_comments(&member.comments, indent + 1);
            self.emit_annotations(&member.annotations, indent + 1);
            let member_name = ident(&member.node.name);
            let (base_type, dims) = split_array_type(&member.node.type_);
            let base_tokens = render_type_tokens(base_type);
            let dims_tokens = render_array_dimensions_tokens(dims);
            let line = quote!(#base_tokens #member_name #dims_tokens ;);
            self.write_tokens_line(indent + 1, line);
        }
        self.write_raw_line(indent, "};");
    }

    fn emit_const(&mut self, const_def: &Commented<ConstDef>, indent: usize) {
        self.emit_comments(&const_def.comments, indent);
        self.emit_annotations(&const_def.annotations, indent);
        let name = ident(&const_def.node.name);
        let const_type = render_type_tokens(&const_def.node.const_type);
        let value_fragment = render_const_value_fragment(&const_def.node.value);
        let assignment = format_assignment(quote!(const #const_type #name =), &value_fragment);
        self.write_raw_line_with_suffix(indent, &assignment, ";");
    }

    fn emit_import(&mut self, import_def: &Commented<ImportDef>, indent: usize) {
        self.emit_comments(&import_def.comments, indent);
        self.emit_annotations(&import_def.annotations, indent);
        let scope_tokens = render_import_scope_tokens(&import_def.node.scope);
        let line = quote!(import #scope_tokens ;);
        self.write_tokens_line(indent, line);
    }

    fn emit_comments(&mut self, comments: &[String], indent: usize) {
        for comment in comments {
            for line in comment.lines() {
                self.write_raw_line(indent, line);
            }
        }
    }

    fn emit_annotations(&mut self, annotations: &[Annotation], indent: usize) {
        for annotation in annotations {
            self.write_indent(indent);
            self.buffer.push('@');
            self.buffer.push_str(&format_scoped_name(&annotation.name));
            self.buffer.push('(');
            if !annotation.params.is_empty() {
                let params = render_annotation_params(annotation);
                self.buffer.push_str(&params);
            }
            self.buffer.push_str(")\n");
        }
    }

    fn write_tokens_line(&mut self, indent: usize, tokens: TokenStream) {
        self.write_tokens_with_suffix(indent, tokens, "");
    }

    fn write_tokens_with_suffix(&mut self, indent: usize, tokens: TokenStream, suffix: &str) {
        self.write_indent(indent);
        self.buffer.push_str(&tokens_to_line(tokens));
        if !suffix.is_empty() {
            self.buffer.push_str(suffix);
        }
        self.buffer.push('\n');
    }

    fn write_raw_line(&mut self, indent: usize, text: &str) {
        self.write_indent(indent);
        self.buffer.push_str(text);
        self.buffer.push('\n');
    }

    fn write_raw_line_with_suffix(&mut self, indent: usize, text: &str, suffix: &str) {
        self.write_indent(indent);
        self.buffer.push_str(text);
        self.buffer.push_str(suffix);
        self.buffer.push('\n');
    }

    fn write_indent(&mut self, indent: usize) {
        for _ in 0..indent {
            self.buffer.push_str("    ");
        }
    }
}

fn render_type_tokens(type_: &Type) -> TokenStream {
    match type_ {
        Type::Long => quote!(long),
        Type::Short => quote!(short),
        Type::UnsignedLong => quote!(uint32),
        Type::UnsignedShort => quote!(uint16),
        Type::LongLong => quote!(long long),
        Type::UnsignedLongLong => quote!(uint64),
        Type::Float => quote!(float),
        Type::Double => quote!(double),
        Type::LongDouble => quote!(long double),
        Type::Boolean => quote!(boolean),
        Type::Char => quote!(char),
        Type::WChar => quote!(wchar),
        Type::Octet => quote!(octet),
        Type::String => quote!(string),
        Type::WString => quote!(wstring),
        Type::Sequence { element_type, size } => {
            let element_tokens = render_type_tokens(element_type);
            if let Some(size) = size {
                let size_literal = Literal::u32_unsuffixed(*size);
                quote!(sequence<#element_tokens, #size_literal>)
            } else {
                quote!(sequence<#element_tokens>)
            }
        }
        Type::Array { element_type, .. } => render_type_tokens(element_type),
        Type::ScopedName(parts) => render_scoped_name_tokens(parts),
    }
}

fn render_array_dimensions_tokens(dims: &[u32]) -> TokenStream {
    let chunks = dims.iter().map(|dim| {
        let literal = Literal::u32_unsuffixed(*dim);
        quote!([#literal])
    });
    quote!(#(#chunks)*)
}

fn render_const_value_fragment(value: &ConstValue) -> LiteralFragment {
    match value {
        ConstValue::Integer(literal) => {
            LiteralFragment::Tokens(literal_string_tokens(&format_integer_literal(literal)))
        }
        ConstValue::Float(f) => {
            LiteralFragment::Tokens(literal_string_tokens(&format_float_literal(*f)))
        }
        ConstValue::Fixed(fixed) => LiteralFragment::Raw(format_fixed_point_literal(fixed)),
        ConstValue::String(s) => {
            let literal = Literal::string(s);
            LiteralFragment::Tokens(quote!(#literal))
        }
        ConstValue::Boolean(true) => LiteralFragment::Tokens(quote!(TRUE)),
        ConstValue::Boolean(false) => LiteralFragment::Tokens(quote!(FALSE)),
        ConstValue::Char(ch) => {
            let literal = Literal::character(*ch);
            LiteralFragment::Tokens(quote!(#literal))
        }
        ConstValue::ScopedName(parts) => LiteralFragment::Tokens(render_scoped_name_tokens(parts)),
    }
}

fn render_import_scope_tokens(scope: &ImportScope) -> TokenStream {
    match scope {
        ImportScope::Scoped(path) => render_scoped_name_tokens(path),
        ImportScope::String(path) => {
            let literal = Literal::string(path);
            quote!(#literal)
        }
    }
}

fn render_annotation_params(annotation: &Annotation) -> String {
    let mut rendered = Vec::new();
    for param in &annotation.params {
        let AnnotationParam::Named { name, value } = param;
        let ident = ident(name);
        let fragment = render_const_value_fragment(value);
        let assignment = format_assignment(quote!(#ident =), &fragment);
        rendered.push(assignment.trim().to_string());
    }
    rendered.join(", ")
}

fn format_assignment(lhs: TokenStream, value: &LiteralFragment) -> String {
    let mut lhs_text = tokens_to_line(lhs);
    if !lhs_text.ends_with(' ') && !lhs_text.is_empty() {
        lhs_text.push(' ');
    }
    lhs_text.push_str(&value.to_string());
    lhs_text
}

fn format_fixed_point_literal(literal: &FixedPointLiteral) -> String {
    let mut output = String::new();
    if literal.negative {
        output.push('-');
    }
    let digits = literal.digits.as_str();
    let scale = literal.scale as usize;
    if scale == 0 {
        output.push_str(digits);
    } else {
        let precision = digits.len();
        if precision > scale {
            output.push_str(&digits[..precision - scale]);
        } else {
            output.push('0');
        }
        output.push('.');
        if precision < scale {
            output.extend(std::iter::repeat_n('0', scale - precision));
            output.push_str(digits);
        } else {
            output.push_str(&digits[precision - scale..]);
        }
    }
    output.push('d');
    output
}

enum LiteralFragment {
    Tokens(TokenStream),
    Raw(String),
}

impl fmt::Display for LiteralFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralFragment::Tokens(tokens) => f.write_str(&tokens_to_line(tokens.clone())),
            LiteralFragment::Raw(raw) => f.write_str(raw),
        }
    }
}

fn ident(name: &str) -> Ident {
    Ident::new(name, Span::call_site())
}

fn literal_string_tokens(text: &str) -> TokenStream {
    TokenStream::from_str(text).expect("literal should parse into a token stream")
}

fn split_array_type(type_: &Type) -> (&Type, &[u32]) {
    match type_ {
        Type::Array {
            element_type,
            dimensions,
        } => (element_type.as_ref(), dimensions),
        _ => (type_, &[]),
    }
}

fn format_integer_literal(literal: &IntegerLiteral) -> String {
    match literal.base {
        IntegerBase::Decimal => literal.value.to_string(),
        IntegerBase::Octal => format_with_base_prefix(literal.value, 8, "0"),
        IntegerBase::Hexadecimal => format_with_base_prefix(literal.value, 16, "0x"),
    }
}

fn format_with_base_prefix(value: i64, radix: u32, prefix: &str) -> String {
    let signed = value as i128;
    let magnitude = (if value < 0 { -signed } else { signed }) as u128;
    let digits = match radix {
        8 => format!("{:o}", magnitude),
        16 => format!("{:X}", magnitude),
        _ => unreachable!(),
    };
    if value < 0 {
        format!("-{}{}", prefix, digits)
    } else {
        format!("{}{}", prefix, digits)
    }
}

fn format_float_literal(value: f64) -> String {
    if value.fract() == 0.0 {
        format!("{:.1}", value)
    } else {
        value.to_string()
    }
}

fn render_scoped_name_tokens(parts: &[String]) -> TokenStream {
    let mut iter = parts.iter();
    let first = match iter.next() {
        Some(name) => ident(name),
        None => return TokenStream::new(),
    };
    let rest: Vec<Ident> = iter.map(|name| ident(name)).collect();
    quote!(#first #( :: #rest )*)
}

fn format_scoped_name(parts: &[String]) -> String {
    parts.join("::")
}

fn tokens_to_line(tokens: TokenStream) -> String {
    format_stream(tokens).trim().to_string()
}

fn format_stream(tokens: TokenStream) -> String {
    let mut writer = TokenLineWriter::new();
    writer.push_stream(tokens);
    writer.finish()
}

struct TokenLineWriter {
    output: String,
    prev_kind: PrevKind,
    pending_double_colon: bool,
}

impl TokenLineWriter {
    fn new() -> Self {
        Self {
            output: String::new(),
            prev_kind: PrevKind::None,
            pending_double_colon: false,
        }
    }

    fn push_stream(&mut self, tokens: TokenStream) {
        for token in tokens {
            self.push_token(token);
        }
    }

    fn push_token(&mut self, token: TokenTree) {
        match token {
            TokenTree::Ident(ident) => {
                if matches!(
                    self.prev_kind,
                    PrevKind::Ident | PrevKind::Literal | PrevKind::Closing
                ) {
                    self.output.push(' ');
                }
                self.output.push_str(&ident.to_string());
                self.prev_kind = PrevKind::Ident;
            }
            TokenTree::Literal(literal) => {
                if matches!(
                    self.prev_kind,
                    PrevKind::Ident | PrevKind::Literal | PrevKind::Closing
                ) {
                    self.output.push(' ');
                }
                self.output.push_str(&literal.to_string());
                self.prev_kind = PrevKind::Literal;
            }
            TokenTree::Punct(punct) => {
                let ch = punct.as_char();
                match ch {
                    ',' => {
                        self.trim_trailing_space();
                        self.output.push(',');
                        self.output.push(' ');
                        self.prev_kind = PrevKind::Other;
                    }
                    ';' => {
                        self.trim_trailing_space();
                        self.output.push(';');
                        self.prev_kind = PrevKind::Other;
                    }
                    '=' => {
                        self.trim_trailing_space();
                        self.output.push(' ');
                        self.output.push('=');
                        self.output.push(' ');
                        self.prev_kind = PrevKind::Other;
                    }
                    '<' | '(' | '[' => {
                        self.trim_trailing_space();
                        self.output.push(ch);
                        self.prev_kind = PrevKind::Other;
                    }
                    '>' | ')' | ']' => {
                        self.trim_trailing_space();
                        self.output.push(ch);
                        self.prev_kind = PrevKind::Closing;
                    }
                    ':' => {
                        if punct.spacing() == Spacing::Joint {
                            self.output.push(':');
                            self.pending_double_colon = true;
                        } else if self.pending_double_colon {
                            self.output.push(':');
                            self.pending_double_colon = false;
                        } else {
                            if !self.output.ends_with(' ') && !self.output.is_empty() {
                                self.output.push(' ');
                            }
                            self.output.push(':');
                            self.output.push(' ');
                        }
                        self.prev_kind = PrevKind::Other;
                    }
                    _ => {
                        self.output.push(ch);
                        self.prev_kind = PrevKind::Other;
                    }
                }
            }
            TokenTree::Group(group) => match group.delimiter() {
                Delimiter::Bracket => {
                    self.output.push('[');
                    let inner = format_stream(group.stream());
                    self.output.push_str(inner.trim());
                    self.trim_trailing_space();
                    self.output.push(']');
                    self.prev_kind = PrevKind::Closing;
                }
                Delimiter::Parenthesis => {
                    self.output.push('(');
                    let inner = format_stream(group.stream());
                    self.output.push_str(inner.trim());
                    self.trim_trailing_space();
                    self.output.push(')');
                    self.prev_kind = PrevKind::Closing;
                }
                Delimiter::Brace => {
                    self.output.push('{');
                    let inner = format_stream(group.stream());
                    self.output.push_str(inner.trim());
                    self.output.push('}');
                    self.prev_kind = PrevKind::Closing;
                }
                Delimiter::None => {
                    let inner = format_stream(group.stream());
                    self.output.push_str(&inner);
                }
            },
        }
    }

    fn trim_trailing_space(&mut self) {
        while self.output.ends_with(' ') {
            self.output.pop();
        }
    }

    fn finish(mut self) -> String {
        self.trim_trailing_space();
        self.output
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum PrevKind {
    None,
    Ident,
    Literal,
    Closing,
    Other,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse_idl;
    use std::{
        fs,
        path::PathBuf,
        time::{SystemTime, UNIX_EPOCH},
    };

    #[test]
    fn generates_round_trip_output() {
        let defs = load_fixture("typedefs.idl");
        let emitted = generate_idl(&defs);
        let reparsed = parse_idl(&emitted).expect("generated IDL should parse");
        assert_eq!(
            reparsed, defs,
            "reparsed AST should match the original definitions"
        );
    }

    #[test]
    fn writes_generated_file_to_disk() {
        let defs = load_fixture("struct_basic.idl");
        let mut path = PathBuf::from("target/tmp");
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time should be valid")
            .as_nanos();
        path.push(format!("generated_{timestamp}.idl"));
        write_idl_file(&defs, &path).expect("write_idl_file should succeed");
        let written = fs::read_to_string(&path).expect("written file should exist");
        assert!(
            !written.is_empty(),
            "generated IDL file should contain content"
        );
        let reparsed = parse_idl(&written).expect("written IDL should parse");
        assert_eq!(
            reparsed, defs,
            "written AST should match source definitions"
        );
        let _ = fs::remove_file(&path);
    }

    fn load_fixture(name: &str) -> Vec<Definition> {
        let path = format!("tests/fixtures/{name}");
        let input = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read fixture {}: {}", path, err));
        parse_idl(&input).expect("fixture should parse successfully")
    }
}
