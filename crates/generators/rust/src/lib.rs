use std::{collections::HashMap, fs, io, path::Path};

use blueberry_ast::{
    Commented, ConstDef, ConstValue, Definition, EnumDef, MessageDef, ModuleDef, StructDef, Type,
    TypeDef,
};
use blueberry_codegen_core::{CodegenError, GeneratedFile};
use genco::lang::rust::Tokens;
use genco::quote;

const OUTPUT_PATH: &str = "rust/blueberry_generated.rs";
const RUNTIME_PATH: &str = "rust/blueberry_generated/runtime.rs";

/// Generate Rust code for the provided IDL definitions.
pub fn generate_rust(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let generator = RustGenerator::new(definitions);
    let root_file = generator.generate_root(definitions);
    let runtime_file = generator.generate_runtime();
    Ok(vec![
        GeneratedFile {
            path: OUTPUT_PATH.to_string(),
            contents: root_file,
        },
        GeneratedFile {
            path: RUNTIME_PATH.to_string(),
            contents: runtime_file,
        },
    ])
}

/// Write the root generated Rust module to disk.
///
/// Note: the Rust generator emits multiple files. This helper writes only the
/// top-level module; use `generate_rust` to retrieve all files and persist them as needed.
pub fn write_rust_file<P: AsRef<Path>>(definitions: &[Definition], path: P) -> io::Result<()> {
    let contents = RustGenerator::new(definitions).generate_root(definitions);
    if let Some(parent) = path.as_ref().parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)?;
    }
    fs::write(path, contents)
}

struct RustGenerator {
    registry: TypeRegistry,
}

impl RustGenerator {
    fn new(definitions: &[Definition]) -> Self {
        Self {
            registry: TypeRegistry::new(definitions),
        }
    }

    fn doc_attributes(&self, comments: &[String]) -> Vec<Tokens> {
        comments
            .iter()
            .flat_map(|comment| {
                if comment.is_empty() {
                    vec![quote!(#[doc = ""])]
                } else {
                    comment
                        .lines()
                        .map(|line| {
                            let content = format!(" {}", line.trim_end());
                            quote!(#[doc = $(format!("\"{content}\""))])
                        })
                        .collect()
                }
            })
            .collect()
    }

    fn generate_root(&self, definitions: &[Definition]) -> String {
        let tokens = self.generate_root_tokens(definitions);
        tokens.to_file_string().expect("render rust root")
    }

    fn generate_runtime(&self) -> String {
        let tokens = self.runtime_module_items();
        tokens.to_file_string().expect("render rust runtime")
    }

    fn generate_root_tokens(&self, definitions: &[Definition]) -> Tokens {
        let defs = self.emit_definitions(definitions, &[]);
        let tests = self.emit_tests(definitions);
        quote! {
            pub mod blueberry_generated {
                pub mod runtime;
                pub use runtime::CdrEncoding;
                $(for def in defs => $def)
            }

            $tests
        }
    }

    fn emit_tests(&self, _definitions: &[Definition]) -> Tokens {
        let mut struct_paths: Vec<Vec<String>> = self.registry.structs.keys().cloned().collect();
        if struct_paths.is_empty() {
            return Tokens::new();
        }
        struct_paths.sort();
        let tests: Vec<Tokens> = struct_paths
            .into_iter()
            .map(|path| {
                let suffix = path
                    .iter()
                    .map(|segment| segment.to_lowercase())
                    .collect::<Vec<_>>()
                    .join("_");
                let test_name = format!("test_default_{suffix}");
                let type_ident = path.last().expect("struct has name").to_string();
                let type_ident_clone = type_ident.clone();
                let type_path = self.relative_path(&path, &[]);
                quote! {
                    #[test]
                    fn $test_name() {
                        use crate::blueberry_generated::runtime::CdrEncoding;
                        use $type_path;

                        let value = $type_ident::default();

                        let bytes = value.to_payload().expect("to_payload");
                        let hex: String = bytes.iter().map(|b| format!("{:02X}", b)).collect();
                        println!("HEX: 0x{}", hex);

                        let decoded = <$type_ident_clone as CdrEncoding>::from_payload(&bytes).expect("from_payload");

                        assert_eq!(value, decoded);
                        println!("{:?}", decoded);
                    }
                }
            })
            .collect();

        quote! {
            #[cfg(test)]
            mod generated_tests {
                $(for t in tests => $t)
            }
        }
    }

    fn runtime_module_items(&self) -> Tokens {
        quote! {
            use cdr::{deserialize, serialize, CdrLe, Infinite};
            use serde::{de::DeserializeOwned, Serialize};

            #[derive(Debug)]
            pub enum Error {
                Cdr(cdr::Error),
                InvalidEnum { name: &'static str, value: i64 },
            }

            impl From<cdr::Error> for Error {
                fn from(err: cdr::Error) -> Self {
                    Error::Cdr(err)
                }
            }

            pub trait CdrEncoding: Serialize + DeserializeOwned {
                fn to_payload(&self) -> Result<Vec<u8>, Error> {
                    serialize::<_, _, CdrLe>(self, Infinite).map_err(Error::Cdr)
                }

                fn from_payload(bytes: &[u8]) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    deserialize(bytes).map_err(Error::Cdr)
                }
            }

            impl<T> CdrEncoding for T where T: Serialize + DeserializeOwned {}
        }
    }

    fn emit_definitions(&self, defs: &[Definition], scope: &[String]) -> Vec<Tokens> {
        defs.iter()
            .map(|def| match def {
                Definition::ModuleDef(module) => self.emit_module(module, scope),
                Definition::TypeDef(typedef) => self.emit_typedef(typedef, scope),
                Definition::EnumDef(enum_def) => self.emit_enum(enum_def, scope),
                Definition::StructDef(struct_def) => self.emit_struct(struct_def, scope),
                Definition::MessageDef(message_def) => self.emit_message(message_def, scope),
                Definition::ConstDef(const_def) => self.emit_const(const_def, scope),
                Definition::ImportDef(_) => Tokens::new(),
            })
            .collect()
    }

    fn emit_module(&self, module: &Commented<ModuleDef>, scope: &[String]) -> Tokens {
        let ident = module.node.name.clone();
        let mut new_scope = scope.to_vec();
        new_scope.push(module.node.name.clone());
        let defs = self.emit_definitions(&module.node.definitions, &new_scope);
        let docs = self.doc_attributes(&module.comments);
        quote! {
            $(for doc in docs => $doc)
            pub mod $ident {
                use crate::blueberry_generated::runtime;
                $(for def in defs => $def)
            }
        }
    }

    fn emit_typedef(&self, typedef: &Commented<TypeDef>, scope: &[String]) -> Tokens {
        let name = typedef.node.name.clone();
        let resolved = self.registry.resolve_type(&typedef.node.base_type, scope);
        let ty = self.render_type(&resolved, scope);
        let docs = self.doc_attributes(&typedef.comments);
        quote! {
            $(for doc in docs => $doc)
            pub type $name = $ty;
        }
    }

    fn emit_enum(&self, enum_def: &Commented<EnumDef>, scope: &[String]) -> Tokens {
        let ident = enum_def.node.name.clone();
        let repr_ty = enum_def
            .node
            .base_type
            .as_ref()
            .map(|t| self.render_type(t, scope))
            .unwrap_or_else(|| quote!(u32));
        let variants: Vec<Tokens> = enum_def
            .node
            .enumerators
            .iter()
            .enumerate()
            .map(|(idx, member)| {
                let name = member.name.clone();
                let docs = self.doc_attributes(&member.comments);
                let default_attr = if idx == 0 {
                    self.default_attr_tokens()
                } else {
                    Tokens::new()
                };
                if let Some(value) = &member.value {
                    let literal = self.render_const(value, scope);
                    quote! {
                        $(for doc in docs => $doc)
                        $default_attr
                        $name = $literal,
                    }
                } else {
                    quote! {
                        $(for doc in docs => $doc)
                        $default_attr
                        $name,
                    }
                }
            })
            .collect();
        let matches: Vec<Tokens> = enum_def
            .node
            .enumerators
            .iter()
            .map(|member| {
                let name = member.name.clone();
                let literal = if let Some(v) = &member.value {
                    self.render_const(v, scope)
                } else {
                    let repr_clone = repr_ty.clone();
                    let name_clone = name.clone();
                    quote!(Self::$name_clone as $repr_clone)
                };
                let name_match = name.clone();
                quote!( $literal => Ok(Self::$name_match), )
            })
            .collect();
        let docs = self.doc_attributes(&enum_def.comments);
        let repr_ty_attr = repr_ty.clone();
        let repr_ty_try_header = repr_ty.clone();
        let repr_ty_try_value = repr_ty.clone();
        let repr_ty_from_header = repr_ty.clone();
        let repr_ty_from_body = repr_ty.clone();
        let ident_try_header = ident.clone();
        let ident_try_body = ident.clone();
        let ident_from_header = ident.clone();
        let ident_from_body = ident.clone();
        quote! {
            $(for doc in docs => $doc)
            #[repr($repr_ty_attr)]
            #[derive(
                Copy,
                Clone,
                Debug,
                PartialEq,
                Eq,
                Default,
                ::serde::Serialize,
                ::serde::Deserialize
            )]
            pub enum $ident {
                $(for v in variants => $v)
            }

            impl ::core::convert::TryFrom<$repr_ty_try_header> for $ident_try_header {
                type Error = runtime::Error;

                fn try_from(value: $repr_ty_try_value) -> Result<Self, Self::Error> {
                    match value {
                        $(for m in matches => $m)
                        _ => Err(runtime::Error::InvalidEnum { name: stringify!($ident_try_body), value: value as i64 }),
                    }
                }
            }

            impl From<$ident_from_header> for $repr_ty_from_header {
                fn from(value: $ident_from_body) -> Self {
                    value as $repr_ty_from_body
                }
            }
        }
    }

    fn emit_struct(&self, struct_def: &Commented<StructDef>, scope: &[String]) -> Tokens {
        let ident = struct_def.node.name.clone();
        let mut path = scope.to_vec();
        path.push(struct_def.node.name.clone());
        let members = self.registry.collect_struct_members(&path);
        self.emit_struct_like(&ident, &members, &struct_def.comments, scope)
    }

    fn emit_message(&self, message_def: &Commented<MessageDef>, scope: &[String]) -> Tokens {
        let ident = message_def.node.name.clone();
        let mut path = scope.to_vec();
        path.push(message_def.node.name.clone());
        let members = self.registry.collect_message_members(&path);
        self.emit_struct_like(&ident, &members, &message_def.comments, scope)
    }

    fn emit_struct_like(
        &self,
        ident: &str,
        members: &[ResolvedMember],
        comments: &[String],
        scope: &[String],
    ) -> Tokens {
        let fields: Vec<Tokens> = members
            .iter()
            .map(|member| {
                let name = member.name.clone();
                let ty = self.render_type(&member.ty, scope);
                let docs = self.doc_attributes(&member.comments);
                quote! {
                    $(for doc in docs => $doc)
                    pub $name: $ty,
                }
            })
            .collect();
        let docs = self.doc_attributes(comments);

        quote! {
            $(for doc in docs => $doc)
            #[derive(
                Clone,
                Debug,
                PartialEq,
                Default,
                ::serde::Serialize,
                ::serde::Deserialize
            )]
            pub struct $ident {
                $(for f in fields => $f)
            }
        }
    }

    fn emit_const(&self, const_def: &Commented<ConstDef>, scope: &[String]) -> Tokens {
        let name = const_def.node.name.clone();
        let ty = self
            .registry
            .resolve_type(&const_def.node.const_type, scope);
        let mut ty_tokens = self.render_type(&ty, scope);
        let value = self.render_const(&const_def.node.value, scope);
        if matches!(ty, Type::String { .. }) {
            ty_tokens = quote!(&'static str);
        }
        let docs = self.doc_attributes(&const_def.comments);
        quote! {
            $(for doc in docs => $doc)
            pub const $name: $ty_tokens = $value;
        }
    }

    fn render_type(&self, ty: &Type, scope: &[String]) -> Tokens {
        match ty {
            Type::Long => quote!(i32),
            Type::Short => quote!(i16),
            Type::UnsignedShort => quote!(u16),
            Type::UnsignedLong => quote!(u32),
            Type::LongLong => quote!(i64),
            Type::UnsignedLongLong => quote!(u64),
            Type::Float => quote!(f32),
            Type::Double => quote!(f64),
            Type::Boolean => quote!(bool),
            Type::Octet => quote!(u8),
            Type::String { .. } => quote!(String),
            Type::Sequence { element_type, .. } => {
                let inner = self.render_type(element_type, scope);
                quote!(Vec<$inner>)
            }
            Type::ScopedName(path) => self.relative_path(path, scope),
            _ => quote!(String),
        }
    }

    fn render_const(&self, value: &ConstValue, scope: &[String]) -> Tokens {
        match value {
            ConstValue::Integer(lit) => {
                let literal = lit.value;
                quote!($literal)
            }
            ConstValue::Float(f) => {
                let literal = format!("{}", f);
                quote!($literal)
            }
            ConstValue::Fixed(fixed) => {
                let literal = format!("{}", fixed.to_f64());
                quote!($literal)
            }
            ConstValue::Binary(binary) => {
                let literal = binary.to_i64();
                quote!($literal)
            }
            ConstValue::String(text) => {
                quote!($(format!("\"{text}\"")))
            }
            ConstValue::Boolean(true) => quote!(true),
            ConstValue::Boolean(false) => quote!(false),
            ConstValue::Char(ch) => {
                quote!($(format!("'{}'", ch)))
            }
            ConstValue::ScopedName(path) => self.relative_path(path, scope),
            ConstValue::UnaryOp { op, expr } => {
                let inner = self.render_const(expr, scope);
                let op_token = match op {
                    blueberry_ast::UnaryOperator::Plus => quote!(+),
                    blueberry_ast::UnaryOperator::Minus => quote!(-),
                };
                quote!(( $op_token $inner ))
            }
            ConstValue::BinaryOp { op, left, right } => {
                let lhs = self.render_const(left, scope);
                let rhs = self.render_const(right, scope);
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

    fn default_attr_tokens(&self) -> Tokens {
        quote!(#[default])
    }

    fn relative_path(&self, target: &[String], _scope: &[String]) -> Tokens {
        let mut path = String::from("crate::blueberry_generated");
        for segment in target {
            path.push_str("::");
            path.push_str(segment);
        }
        quote!($path)
    }
}

#[derive(Clone)]
struct ResolvedMember {
    name: String,
    ty: Type,
    comments: Vec<String>,
}

#[derive(Clone)]
struct TypedefInfo {
    ty: Type,
    scope: Vec<String>,
}

#[derive(Clone)]
struct StructInfo {
    def: StructDef,
    scope: Vec<String>,
}

#[derive(Clone)]
struct MessageInfo {
    def: MessageDef,
    scope: Vec<String>,
}

#[derive(Default)]
struct TypeRegistry {
    typedefs: HashMap<Vec<String>, TypedefInfo>,
    structs: HashMap<Vec<String>, StructInfo>,
    messages: HashMap<Vec<String>, MessageInfo>,
    enums: HashMap<Vec<String>, Type>,
}

impl TypeRegistry {
    fn new(definitions: &[Definition]) -> Self {
        let mut registry = TypeRegistry::default();
        let mut scope = Vec::new();
        registry.collect(definitions, &mut scope);
        registry
    }

    fn collect(&mut self, defs: &[Definition], scope: &mut Vec<String>) {
        for def in defs {
            match def {
                Definition::ModuleDef(module) => {
                    scope.push(module.node.name.clone());
                    self.collect(&module.node.definitions, scope);
                    scope.pop();
                }
                Definition::TypeDef(typedef) => {
                    let mut path = scope.clone();
                    path.push(typedef.node.name.clone());
                    self.typedefs.insert(
                        path,
                        TypedefInfo {
                            ty: typedef.node.base_type.clone(),
                            scope: scope.clone(),
                        },
                    );
                }
                Definition::StructDef(struct_def) => {
                    let mut path = scope.clone();
                    path.push(struct_def.node.name.clone());
                    self.structs.insert(
                        path,
                        StructInfo {
                            def: struct_def.node.clone(),
                            scope: scope.clone(),
                        },
                    );
                }
                Definition::MessageDef(message_def) => {
                    let mut path = scope.clone();
                    path.push(message_def.node.name.clone());
                    self.messages.insert(
                        path,
                        MessageInfo {
                            def: message_def.node.clone(),
                            scope: scope.clone(),
                        },
                    );
                }
                Definition::EnumDef(enum_def) => {
                    let mut path = scope.clone();
                    path.push(enum_def.node.name.clone());
                    let repr = enum_def
                        .node
                        .base_type
                        .clone()
                        .unwrap_or(Type::UnsignedLong);
                    self.enums.insert(path, repr);
                }
                Definition::ConstDef(_) | Definition::ImportDef(_) => {}
            }
        }
    }

    fn resolve_type(&self, ty: &Type, scope: &[String]) -> Type {
        match ty {
            Type::Sequence { element_type, size } => Type::Sequence {
                element_type: Box::new(self.resolve_type(element_type, scope)),
                size: *size,
            },
            Type::Array {
                element_type,
                dimensions,
            } => {
                let mut resolved = self.resolve_type(element_type, scope);
                for &dim in dimensions.iter().rev() {
                    resolved = Type::Sequence {
                        element_type: Box::new(resolved),
                        size: Some(dim),
                    };
                }
                resolved
            }
            Type::ScopedName(name) => {
                if let [single] = name.as_slice()
                    && let Some(mapped) = map_builtin_ident(single)
                {
                    return mapped;
                }
                if let Some(path) = self.resolve_typedef(name, scope) {
                    let info = self.typedefs.get(&path).expect("typedef info missing");
                    self.resolve_type(&info.ty, &info.scope)
                } else if let Some(path) = self.resolve_struct(name, scope) {
                    Type::ScopedName(path)
                } else if let Some(path) = self.resolve_message(name, scope) {
                    Type::ScopedName(path)
                } else if let Some(path) = self.resolve_enum(name, scope) {
                    Type::ScopedName(path)
                } else {
                    Type::ScopedName(name.clone())
                }
            }
            other => other.clone(),
        }
    }

    fn collect_struct_members(&self, path: &[String]) -> Vec<ResolvedMember> {
        let info = self
            .structs
            .get(path)
            .unwrap_or_else(|| panic!("missing struct {:?}", path));
        let mut members = Vec::new();
        if let Some(base) = &info.def.base
            && let Some(base_path) = self.resolve_struct(base, &info.scope)
        {
            members.extend(self.collect_struct_members(&base_path));
        }
        for member in &info.def.members {
            let ty = self.resolve_type(&member.node.type_, &info.scope);
            members.push(ResolvedMember {
                name: member.node.name.clone(),
                ty,
                comments: member.comments.clone(),
            });
        }
        members
    }

    fn collect_message_members(&self, path: &[String]) -> Vec<ResolvedMember> {
        let info = self
            .messages
            .get(path)
            .unwrap_or_else(|| panic!("missing message {:?}", path));
        let mut members = Vec::new();
        if let Some(base) = &info.def.base
            && let Some(base_path) = self.resolve_message(base, &info.scope)
        {
            members.extend(self.collect_message_members(&base_path));
        }
        for member in &info.def.members {
            let ty = self.resolve_type(&member.node.type_, &info.scope);
            members.push(ResolvedMember {
                name: member.node.name.clone(),
                ty,
                comments: member.comments.clone(),
            });
        }
        members
    }

    fn resolve_typedef(&self, name: &[String], scope: &[String]) -> Option<Vec<String>> {
        self.resolve_path(name, scope, self.typedefs.keys())
    }

    fn resolve_struct(&self, name: &[String], scope: &[String]) -> Option<Vec<String>> {
        self.resolve_path(name, scope, self.structs.keys())
    }

    fn resolve_message(&self, name: &[String], scope: &[String]) -> Option<Vec<String>> {
        self.resolve_path(name, scope, self.messages.keys())
    }

    fn resolve_enum(&self, name: &[String], scope: &[String]) -> Option<Vec<String>> {
        self.resolve_path(name, scope, self.enums.keys())
    }

    fn resolve_path<'a, I>(
        &self,
        name: &[String],
        scope: &[String],
        entries: I,
    ) -> Option<Vec<String>>
    where
        I: IntoIterator<Item = &'a Vec<String>>,
    {
        let paths: Vec<Vec<String>> = entries.into_iter().cloned().collect();
        let lookup: std::collections::HashSet<Vec<String>> = paths.iter().cloned().collect();
        for prefix in (0..=scope.len()).rev() {
            let mut candidate = scope[..prefix].to_vec();
            candidate.extend_from_slice(name);
            if lookup.contains(&candidate) {
                return Some(candidate);
            }
        }
        self.resolve_by_suffix(name, &paths)
    }

    fn resolve_by_suffix(&self, name: &[String], paths: &[Vec<String>]) -> Option<Vec<String>> {
        let matches: Vec<&Vec<String>> = paths.iter().filter(|path| path.ends_with(name)).collect();
        if matches.len() == 1 {
            return Some(matches[0].clone());
        }
        None
    }
}

fn map_builtin_ident(name: &str) -> Option<Type> {
    match name {
        "int8" => Some(Type::Octet),
        "int16" => Some(Type::Short),
        "int32" => Some(Type::Long),
        "int64" => Some(Type::LongLong),
        "uint8" => Some(Type::Octet),
        "uint16" => Some(Type::UnsignedShort),
        "uint32" => Some(Type::UnsignedLong),
        "uint64" => Some(Type::UnsignedLongLong),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use blueberry_parser::parse_idl;
    use std::{fs, path::PathBuf};

    fn load_fixture(relative: &str) -> Vec<Definition> {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let workspace_root = manifest_dir
            .parent()
            .and_then(|path| path.parent())
            .and_then(|path| path.parent())
            .expect("workspace root directory");
        let path = workspace_root.join(relative);
        let contents = fs::read_to_string(path).expect("fixture must exist");
        parse_idl(&contents).expect("fixture should parse")
    }

    #[test]
    fn resolves_scoped_names_across_modules() {
        let definitions = load_fixture("crates/parser/tests/fixtures/blueberry_full.idl");
        let registry = TypeRegistry::new(&definitions);
        let message_path = vec!["Blueberry".to_string(), "VersionMessage".to_string()];
        let members = registry.collect_message_members(&message_path);
        let hardware = members
            .iter()
            .find(|member| member.name == "hardware")
            .expect("hardware field should exist");
        match &hardware.ty {
            Type::ScopedName(path) => {
                assert_eq!(path, &["Blueberry".to_string(), "HwType".to_string()])
            }
            other => panic!("expected scoped name, got {other:?}"),
        }
    }
}
