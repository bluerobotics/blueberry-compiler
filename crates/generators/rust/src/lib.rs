use std::collections::BTreeMap;

use blueberry_ast::{
    Annotation, AnnotationParam, Commented, ConstDef, ConstValue, Definition, EnumDef, MessageDef,
    ModuleDef, StructDef, Type, TypeDef,
};
use blueberry_codegen_core::{
    CodegenError, DEFAULT_MODULE_KEY, GeneratedFile, ResolvedMember, TypeRegistry,
};
use genco::lang::rust::Tokens;
use genco::quote;

struct FileModule {
    scope: Vec<String>,
    definitions: Vec<Definition>,
}

struct MessageInfo {
    variant_name: String,
    rust_path: Vec<String>,
    module_key: u16,
    message_key: u16,
}

/// Generate Rust code for the provided IDL definitions.
pub fn generate_rust(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let generator = RustGenerator::new(definitions);
    generator.generate_files(definitions)
}

struct RustGenerator {
    registry: TypeRegistry,
    definitions: Vec<Definition>,
    flatten_depth: usize,
}

impl RustGenerator {
    fn new(definitions: &[Definition]) -> Self {
        let (_, root_scope) = skip_wrapper_modules(definitions);
        Self {
            registry: TypeRegistry::new(definitions),
            definitions: definitions.to_vec(),
            flatten_depth: root_scope.len() + 1,
        }
    }

    fn flatten_path(&self, path: &[String]) -> Vec<String> {
        if path.len() <= self.flatten_depth + 1 {
            return path.to_vec();
        }
        let mut result = path[..self.flatten_depth].to_vec();
        result.push(path[path.len() - 1].clone());
        result
    }

    fn find_parent_module_key(&self, scope: &[String]) -> Option<u16> {
        find_module_key_for_scope(&self.definitions, scope)
    }

    fn generate_files(
        &self,
        definitions: &[Definition],
    ) -> Result<Vec<GeneratedFile>, CodegenError> {
        let file_modules = partition_into_file_modules(definitions);
        let mut generated = Vec::new();

        let mut inline_defs: Option<Vec<Tokens>> = None;
        for fm in &file_modules {
            if fm.scope.is_empty() {
                inline_defs = Some(self.emit_definitions(&fm.definitions, &fm.scope, None));
            } else {
                let file_path = scope_to_rust_file_path(&fm.scope);
                let contents = self.render_file_module(fm);
                generated.push(GeneratedFile {
                    path: file_path,
                    contents,
                });
            }
        }

        generated.extend(self.generate_mod_files(&file_modules));

        let mut top_level: BTreeMap<String, bool> = BTreeMap::new();
        for fm in &file_modules {
            if let Some(first) = fm.scope.first() {
                let is_leaf = fm.scope.len() == 1;
                top_level
                    .entry(first.clone())
                    .and_modify(|existing| {
                        if !is_leaf {
                            *existing = false;
                        }
                    })
                    .or_insert(is_leaf);
            }
        }
        let root_tokens = self.generate_root_tokens(&top_level, definitions, inline_defs);
        generated.push(GeneratedFile {
            path: "rust/mod.rs".to_string(),
            contents: root_tokens.to_file_string().expect("render rust root"),
        });

        generated.push(GeneratedFile {
            path: "rust/runtime.rs".to_string(),
            contents: self.generate_runtime(),
        });

        Ok(generated)
    }

    fn render_file_module(&self, fm: &FileModule) -> String {
        let parent_mk = self.find_parent_module_key(&fm.scope);
        let defs = self.emit_definitions_flat(&fm.definitions, &fm.scope, parent_mk);
        let tokens: Tokens = quote! {
            use crate::blueberry::runtime;

            $(for def in defs => $def)
        };
        tokens.to_file_string().expect("render rust module")
    }

    fn emit_definitions_flat(
        &self,
        defs: &[Definition],
        scope: &[String],
        parent_module_key: Option<u16>,
    ) -> Vec<Tokens> {
        let mut result = Vec::new();
        let mut seen_names: std::collections::HashSet<String> = std::collections::HashSet::new();
        self.emit_definitions_flat_inner(
            defs,
            scope,
            parent_module_key,
            &mut result,
            &mut seen_names,
        );
        result
    }

    fn emit_definitions_flat_inner(
        &self,
        defs: &[Definition],
        scope: &[String],
        parent_module_key: Option<u16>,
        result: &mut Vec<Tokens>,
        seen_names: &mut std::collections::HashSet<String>,
    ) {
        for def in defs {
            match def {
                Definition::ModuleDef(module) => {
                    let mk =
                        annotation_u16(&module.annotations, "module_key").or(parent_module_key);
                    let mut child_scope = scope.to_vec();
                    child_scope.push(module.node.name.clone());

                    let child_names = collect_definition_names(&module.node.definitions);
                    let has_collision = child_names.iter().any(|n| seen_names.contains(n));

                    if has_collision {
                        result.push(self.emit_module(module, scope, parent_module_key));
                    } else {
                        self.emit_definitions_flat_inner(
                            &module.node.definitions,
                            &child_scope,
                            mk,
                            result,
                            seen_names,
                        );
                    }
                }
                Definition::TypeDef(typedef) => {
                    seen_names.insert(typedef.node.name.clone());
                    result.push(self.emit_typedef(typedef, scope));
                }
                Definition::EnumDef(enum_def) => {
                    seen_names.insert(enum_def.node.name.clone());
                    result.push(self.emit_enum(enum_def, scope));
                }
                Definition::StructDef(struct_def) => {
                    seen_names.insert(struct_def.node.name.clone());
                    result.push(self.emit_struct(struct_def, scope));
                }
                Definition::MessageDef(message_def) => {
                    if annotation_u16(&message_def.annotations, "message_key").is_none() {
                        continue;
                    }
                    seen_names.insert(message_def.node.name.clone());
                    result.push(self.emit_message(message_def, scope, parent_module_key));
                }
                Definition::ConstDef(const_def) => {
                    seen_names.insert(const_def.node.name.clone());
                    result.push(self.emit_const(const_def, scope));
                }
                Definition::ImportDef(_) => {}
            }
        }
    }

    fn generate_runtime(&self) -> String {
        let tokens = self.runtime_module_items();
        tokens.to_file_string().expect("render rust runtime")
    }

    fn generate_root_tokens(
        &self,
        top_level_mods: &BTreeMap<String, bool>,
        definitions: &[Definition],
        inline_defs: Option<Vec<Tokens>>,
    ) -> Tokens {
        let mod_decls: Vec<Tokens> = top_level_mods
            .iter()
            .map(|(name, is_leaf)| {
                let snake = to_snake_case(name);
                let n = name.clone();
                if snake == *name {
                    quote!(pub mod $n;)
                } else if *is_leaf {
                    let path_attr = format!("\"{snake}.rs\"");
                    quote!(
                        #[path = $path_attr]
                        pub mod $n;
                    )
                } else {
                    let path_attr = format!("\"{snake}/mod.rs\"");
                    quote!(
                        #[path = $path_attr]
                        pub mod $n;
                    )
                }
            })
            .collect();

        let tests = self.emit_tests(definitions);

        let inline_tokens = if let Some(defs) = inline_defs {
            quote!($(for def in defs => $def))
        } else {
            Tokens::new()
        };

        let message_enum = self.emit_message_enum(definitions);

        quote! {
            pub mod runtime;
            pub use runtime::BlueberryEncoding;

            $(for d in mod_decls => $d)

            $inline_tokens

            $message_enum

            $tests
        }
    }

    fn emit_message_enum(&self, definitions: &[Definition]) -> Tokens {
        let messages = collect_message_info(definitions, &[], None);
        if messages.is_empty() {
            return Tokens::new();
        }

        let mut seen_names: BTreeMap<String, usize> = BTreeMap::new();
        let mut unique_messages: Vec<(String, &MessageInfo)> = Vec::new();
        for m in &messages {
            let count = seen_names.entry(m.variant_name.clone()).or_insert(0);
            *count += 1;
        }
        for m in &messages {
            let vname = if seen_names[&m.variant_name] > 1 {
                m.rust_path.join("_")
            } else {
                m.variant_name.clone()
            };
            unique_messages.push((vname, m));
        }

        let mut variants = Tokens::new();
        for (vname, m) in &unique_messages {
            let type_path = self.relative_path(&m.rust_path, &[]);
            let v = vname.clone();
            variants.push();
            variants.append(quote!($v($type_path),));
        }

        let mut match_arms = Tokens::new();
        let mut seen_keys: std::collections::HashSet<(u16, u16)> = std::collections::HashSet::new();
        for (vname, m) in &unique_messages {
            if !seen_keys.insert((m.module_key, m.message_key)) {
                continue;
            }
            let mk = format!("0x{:04X}", m.module_key);
            let msgk = format!("0x{:04X}", m.message_key);
            let type_path = self.relative_path(&m.rust_path, &[]);
            let v = vname.clone();
            match_arms.push();
            match_arms.append(quote! {
                ($mk, $msgk) => {
                    let (_, v) = blueberry_serde::deserialize_message::<$type_path>(raw)?;
                    Ok(Message::$v(v))
                }
            });
        }

        let mut key_arms = Tokens::new();
        let mut display_arms = Tokens::new();
        let mut trait_impls = Tokens::new();
        for (vname, m) in &unique_messages {
            let mk = format!("0x{:04X}", m.module_key);
            let msgk = format!("0x{:04X}", m.message_key);
            let mk2 = mk.clone();
            let msgk2 = msgk.clone();
            let v1 = vname.clone();
            key_arms.push();
            key_arms.append(quote!(Message::$v1(_) => ($mk, $msgk),));
            let v2 = vname.clone();
            display_arms.push();
            display_arms.append(quote!(Message::$v2(m) => write!(f, "{:?}", m),));

            let type_path = self.relative_path(&m.rust_path, &[]);
            trait_impls.push();
            trait_impls.append(quote! {
                impl BlueberryMessage for $type_path {
                    const MODULE_KEY: u16 = $mk2;
                    const MESSAGE_KEY: u16 = $msgk2;
                }
            });
        }

        quote! {
            pub trait BlueberryMessage {
                const MODULE_KEY: u16;
                const MESSAGE_KEY: u16;
            }

            pub fn request_packet<T: BlueberryMessage>() -> Vec<u8> {
                let msg = blueberry_serde::empty_message(T::MODULE_KEY, T::MESSAGE_KEY);
                blueberry_serde::serialize_packet(&[&msg]).expect("serialize packet")
            }

            $trait_impls

            #[derive(Debug, Clone)]
            pub enum Message {
                $variants
                Unknown { module_key: u16, message_key: u16, body: Vec<u8> },
            }

            impl Message {
                pub fn keys(&self) -> (u16, u16) {
                    match self {
                        $key_arms
                        Message::Unknown { module_key, message_key, .. } => (*module_key, *message_key),
                    }
                }

                pub fn request_packet(&self) -> Vec<u8> {
                    let (mk, msgk) = self.keys();
                    let msg = blueberry_serde::empty_message(mk, msgk);
                    blueberry_serde::serialize_packet(&[&msg]).expect("serialize packet")
                }

                pub fn from_raw(raw: &[u8]) -> Result<Self, blueberry_serde::Error> {
                    let header = blueberry_serde::MessageHeader::decode(raw)
                        .ok_or(blueberry_serde::Error::InvalidHeader)?;
                    match (header.module_key, header.message_key) {
                        $match_arms
                        (m, k) => Ok(Message::Unknown {
                            module_key: m,
                            message_key: k,
                            body: raw[blueberry_serde::HEADER_SIZE..].to_vec(),
                        }),
                    }
                }

                pub fn extract_packets(buf: &mut Vec<u8>) -> Vec<Self> {
                    let mut results = Vec::new();
                    loop {
                        let Some(idx) = buf.windows(4).position(|w| w == blueberry_serde::PACKET_MAGIC) else {
                            break;
                        };
                        if idx > 0 {
                            buf.drain(..idx);
                        }
                        if buf.len() < 8 {
                            break;
                        }
                        let length_words = u16::from_le_bytes([buf[4], buf[5]]) as usize;
                        let packet_len = length_words * 4;
                        if packet_len < 8 || buf.len() < packet_len {
                            break;
                        }
                        match blueberry_serde::deserialize_packet(&buf[..packet_len]) {
                            Ok((_pkt_hdr, raw_messages)) => {
                                for raw_msg in raw_messages {
                                    match Self::from_raw(raw_msg) {
                                        Ok(msg) => results.push(msg),
                                        Err(_) => {}
                                    }
                                }
                                buf.drain(..packet_len);
                            }
                            Err(_) => {
                                buf.drain(..4);
                            }
                        }
                    }
                    results
                }
            }

            impl ::core::fmt::Display for Message {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    match self {
                        $display_arms
                        Message::Unknown { module_key, message_key, body } => {
                            write!(f, "Unknown(0x{:04X}, 0x{:04X}): {} bytes", module_key, message_key, body.len())
                        }
                    }
                }
            }
        }
    }

    fn generate_mod_files(&self, file_modules: &[FileModule]) -> Vec<GeneratedFile> {
        let mut dir_children: BTreeMap<Vec<String>, BTreeMap<String, bool>> = BTreeMap::new();

        for fm in file_modules {
            for depth in 0..fm.scope.len() {
                let dir_scope = fm.scope[..depth].to_vec();
                let child_name = fm.scope[depth].clone();
                let is_leaf = depth == fm.scope.len() - 1;
                dir_children
                    .entry(dir_scope)
                    .or_default()
                    .entry(child_name)
                    .and_modify(|existing| {
                        if !is_leaf {
                            *existing = false;
                        }
                    })
                    .or_insert(is_leaf);
            }
        }

        let mut files = Vec::new();

        for (dir_scope, children) in &dir_children {
            if dir_scope.is_empty() {
                continue;
            }

            let mut declarations: Vec<Tokens> = Vec::new();
            for (name, is_leaf) in children {
                let snake = to_snake_case(name);
                let n = name.clone();
                if snake == *name {
                    declarations.push(quote!(pub mod $n;));
                } else if *is_leaf {
                    let path_attr = format!("\"{snake}.rs\"");
                    declarations.push(quote!(
                        #[path = $path_attr]
                        pub mod $n;
                    ));
                } else {
                    let path_attr = format!("\"{snake}/mod.rs\"");
                    declarations.push(quote!(
                        #[path = $path_attr]
                        pub mod $n;
                    ));
                }
            }

            let dir_path = scope_to_dir_path(dir_scope);
            let tokens: Tokens = quote! {
                $(for d in declarations => $d)
            };

            files.push(GeneratedFile {
                path: format!("{dir_path}/mod.rs"),
                contents: tokens.to_file_string().expect("render mod.rs"),
            });
        }

        files
    }

    fn doc_attributes(&self, _comments: &[String]) -> Vec<Tokens> {
        Vec::new()
    }

    fn emit_tests(&self, _definitions: &[Definition]) -> Tokens {
        let mut struct_paths: Vec<Vec<String>> = self.registry.struct_paths();
        if struct_paths.is_empty() {
            return Tokens::new();
        }
        struct_paths.sort();
        let mut tests = Tokens::new();
        for path in struct_paths {
            let suffix = path
                .iter()
                .map(|segment| segment.to_lowercase())
                .collect::<Vec<_>>()
                .join("_");
            let test_name = format!("test_default_{suffix}");
            let type_ident = path.last().expect("struct has name").to_string();
            let type_ident_clone = type_ident.clone();
            let type_path = self.relative_path(&path, &[]);
            tests.push();
            tests.append(quote! {
                #[test]
                fn $test_name() {
                    use crate::blueberry::runtime::BlueberryEncoding;
                    use $type_path;

                    let value = $type_ident::default();

                    let bytes = value.to_payload().expect("to_payload");
                    let hex: String = bytes.iter().map(|b| format!("{:02X}", b)).collect();
                    println!("HEX: 0x{}", hex);

                    let decoded = <$type_ident_clone as BlueberryEncoding>::from_payload(&bytes).expect("from_payload");

                    assert_eq!(value, decoded);
                    println!("{:?}", decoded);
                }
            });
        }

        quote! {
            #[cfg(test)]
            mod generated_tests {
                $tests
            }
        }
    }

    fn runtime_module_items(&self) -> Tokens {
        quote! {
            use serde::{de::DeserializeOwned, Serialize};

            #[derive(Debug)]
            pub enum Error {
                Serde(blueberry_serde::Error),
                InvalidEnum { name: &'static str, value: i64 },
            }

            impl From<blueberry_serde::Error> for Error {
                fn from(err: blueberry_serde::Error) -> Self {
                    Error::Serde(err)
                }
            }

            pub trait BlueberryEncoding: Serialize + DeserializeOwned {
                fn to_payload(&self) -> Result<Vec<u8>, Error> {
                    blueberry_serde::serialize(self).map_err(Error::Serde)
                }

                fn from_payload(bytes: &[u8]) -> Result<Self, Error>
                where
                    Self: Sized,
                {
                    blueberry_serde::deserialize(bytes).map_err(Error::Serde)
                }

                fn to_message(&self, module_key: u16, message_key: u16) -> Result<Vec<u8>, Error> {
                    blueberry_serde::serialize_message(self, module_key, message_key)
                        .map_err(Error::Serde)
                }

                fn from_message(bytes: &[u8]) -> Result<(blueberry_serde::MessageHeader, Self), Error>
                where
                    Self: Sized,
                {
                    blueberry_serde::deserialize_message(bytes).map_err(Error::Serde)
                }
            }

            impl<T> BlueberryEncoding for T where T: Serialize + DeserializeOwned {}

            pub fn format_topic(template: &str) -> String {
                template.to_string()
            }
        }
    }

    fn emit_definitions(
        &self,
        defs: &[Definition],
        scope: &[String],
        parent_module_key: Option<u16>,
    ) -> Vec<Tokens> {
        let mut merged_modules: BTreeMap<String, Vec<&Commented<ModuleDef>>> = BTreeMap::new();
        let mut non_module_defs = Vec::new();

        for def in defs {
            match def {
                Definition::ModuleDef(module) => {
                    merged_modules
                        .entry(module.node.name.clone())
                        .or_default()
                        .push(module);
                }
                _ => non_module_defs.push(def),
            }
        }

        let mut result: Vec<Tokens> = non_module_defs
            .iter()
            .map(|def| match def {
                Definition::TypeDef(typedef) => self.emit_typedef(typedef, scope),
                Definition::EnumDef(enum_def) => self.emit_enum(enum_def, scope),
                Definition::StructDef(struct_def) => self.emit_struct(struct_def, scope),
                Definition::MessageDef(message_def) => {
                    self.emit_message(message_def, scope, parent_module_key)
                }
                Definition::ConstDef(const_def) => self.emit_const(const_def, scope),
                Definition::ImportDef(_) => Tokens::new(),
                _ => Tokens::new(),
            })
            .collect();

        for (name, modules) in &merged_modules {
            let mut merged_defs: Vec<Definition> = Vec::new();
            let mut annotations = Vec::new();
            let mut comments = Vec::new();
            for m in modules {
                merged_defs.extend(m.node.definitions.clone());
                annotations.extend(m.annotations.clone());
                comments.extend(m.comments.clone());
            }
            let merged = Commented {
                comments,
                annotations,
                node: ModuleDef {
                    name: name.clone(),
                    definitions: merged_defs,
                },
            };
            result.push(self.emit_module(&merged, scope, parent_module_key));
        }

        result
    }

    fn emit_module(
        &self,
        module: &Commented<ModuleDef>,
        scope: &[String],
        parent_module_key: Option<u16>,
    ) -> Tokens {
        let ident = module.node.name.clone();
        let mut new_scope = scope.to_vec();
        new_scope.push(module.node.name.clone());
        let mk = annotation_u16(&module.annotations, "module_key").or(parent_module_key);
        let defs = self.emit_definitions(&module.node.definitions, &new_scope, mk);
        let docs = self.doc_attributes(&module.comments);
        quote! {
            $(for doc in docs => $doc)
            pub mod $ident {
                use crate::blueberry::runtime;
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
        let mut seen_discriminants: std::collections::HashSet<i128> =
            std::collections::HashSet::new();
        let deduped_enumerators: Vec<_> = enum_def
            .node
            .enumerators
            .iter()
            .filter(|member| {
                if let Some(value) = &member.value
                    && let Some(v) = self.const_to_i128(value)
                {
                    return seen_discriminants.insert(v);
                }
                true
            })
            .collect();
        let variants: Vec<Tokens> = deduped_enumerators
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
        let matches: Vec<Tokens> = deduped_enumerators
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
                ::serde_repr::Serialize_repr,
                ::serde_repr::Deserialize_repr
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
        let mut members = self.registry.collect_struct_members(&path);
        self.registry.sort_members_by_alignment(&mut members);
        self.emit_struct_like(
            &ident,
            &members,
            &struct_def.comments,
            &struct_def.annotations,
            scope,
            None,
        )
    }

    fn emit_message(
        &self,
        message_def: &Commented<MessageDef>,
        scope: &[String],
        parent_module_key: Option<u16>,
    ) -> Tokens {
        let ident = message_def.node.name.clone();
        let mut path = scope.to_vec();
        path.push(message_def.node.name.clone());
        let mut members = self.registry.collect_message_members(&path);
        self.registry.sort_members_by_alignment(&mut members);
        let inherited_module_key =
            annotation_u16(&message_def.annotations, "module_key").or(parent_module_key);
        self.emit_struct_like(
            &ident,
            &members,
            &message_def.comments,
            &message_def.annotations,
            scope,
            inherited_module_key,
        )
    }

    fn emit_struct_like(
        &self,
        ident: &str,
        members: &[ResolvedMember],
        comments: &[String],
        annotations: &[Annotation],
        scope: &[String],
        inherited_module_key: Option<u16>,
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
        let topic_impl = self.topic_impl(ident, annotations, inherited_module_key);

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

            $topic_impl
        }
    }

    fn topic_impl(
        &self,
        ident: &str,
        annotations: &[Annotation],
        inherited_module_key: Option<u16>,
    ) -> Tokens {
        let topic = self.topic_value(annotations);
        let message_key = annotation_u16(annotations, "message_key");
        let module_key = annotation_u16(annotations, "module_key")
            .or(inherited_module_key)
            .or_else(|| message_key.map(|_| DEFAULT_MODULE_KEY));

        if topic.is_none() && module_key.is_none() && message_key.is_none() {
            return Tokens::new();
        }

        let topic_tokens = if let Some(topic) = topic {
            let topic_literal = format!("{topic:?}");
            quote! {
                pub const TOPIC_TEMPLATE: &'static str = $topic_literal;

                pub fn topic() -> String {
                    runtime::format_topic(Self::TOPIC_TEMPLATE)
                }
            }
        } else {
            Tokens::new()
        };

        let mk_tokens = if let Some(mk) = module_key {
            let mk_literal = format!("0x{mk:04X}");
            quote! { pub const MODULE_KEY: u16 = $mk_literal; }
        } else {
            Tokens::new()
        };

        let msg_key_tokens = if let Some(msgk) = message_key {
            let msgk_literal = format!("0x{msgk:04X}");
            quote! { pub const MESSAGE_KEY: u16 = $msgk_literal; }
        } else {
            Tokens::new()
        };

        quote! {
            impl $ident {
                $mk_tokens
                $msg_key_tokens
                $topic_tokens
            }
        }
    }

    fn topic_value(&self, annotations: &[Annotation]) -> Option<String> {
        annotations
            .iter()
            .find(|annotation| {
                annotation
                    .name
                    .last()
                    .map(|segment| segment.eq_ignore_ascii_case("topic"))
                    .unwrap_or(false)
            })
            .and_then(|annotation| self.annotation_string(annotation))
    }

    fn annotation_string(&self, annotation: &Annotation) -> Option<String> {
        annotation.params.iter().find_map(|param| match param {
            AnnotationParam::Named { name, value } if name.eq_ignore_ascii_case("value") => {
                self.const_string(value)
            }
            AnnotationParam::Positional(value) => self.const_string(value),
            AnnotationParam::Named { value, .. } => self.const_string(value),
        })
    }

    fn const_to_i128(&self, value: &ConstValue) -> Option<i128> {
        match value {
            ConstValue::Integer(lit) => Some(lit.value),
            ConstValue::Binary(b) => Some(b.to_i128()),
            _ => None,
        }
    }

    fn const_string(&self, value: &ConstValue) -> Option<String> {
        match value {
            ConstValue::String(text) => Some(text.clone()),
            _ => None,
        }
    }

    fn emit_const(&self, const_def: &Commented<ConstDef>, scope: &[String]) -> Tokens {
        let name = const_def.node.name.clone();
        let ty = self
            .registry
            .resolve_type(&const_def.node.const_type, scope);
        let mut ty_tokens = self.render_type(&ty, scope);
        let value = self.render_const_typed(&const_def.node.value, scope, &ty);
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
            Type::Double | Type::LongDouble => quote!(f64),
            Type::Boolean => quote!(bool),
            Type::Octet => quote!(u8),
            Type::Char | Type::WChar => quote!(char),
            Type::String { .. } => quote!(String),
            Type::Sequence { element_type, .. } => {
                let inner = self.render_type(element_type, scope);
                quote!(Vec<$inner>)
            }
            Type::ScopedName(path) => self.relative_path(path, scope),
            _ => quote!(String),
        }
    }

    fn is_float_type(ty: &Type) -> bool {
        matches!(ty, Type::Float | Type::Double | Type::LongDouble)
    }

    fn is_integer_type(ty: &Type) -> bool {
        matches!(
            ty,
            Type::Long
                | Type::Short
                | Type::UnsignedShort
                | Type::UnsignedLong
                | Type::LongLong
                | Type::UnsignedLongLong
                | Type::Octet
        )
    }

    fn render_const_typed(&self, value: &ConstValue, scope: &[String], target_ty: &Type) -> Tokens {
        match value {
            ConstValue::Integer(lit) if Self::is_float_type(target_ty) => {
                let literal = format!("{}.0", lit.value);
                quote!($literal)
            }
            ConstValue::Float(f) if Self::is_integer_type(target_ty) => {
                let literal = *f as i128;
                quote!($literal)
            }
            ConstValue::Fixed(fixed) if Self::is_float_type(target_ty) => {
                let mut literal = format!("{}", fixed.to_f64());
                if !literal.contains('.') {
                    literal.push_str(".0");
                }
                quote!($literal)
            }
            ConstValue::Fixed(fixed) if Self::is_integer_type(target_ty) => {
                let literal = fixed.to_f64() as i128;
                quote!($literal)
            }
            ConstValue::UnaryOp { op, expr } => {
                let inner = self.render_const_typed(expr, scope, target_ty);
                let op_token = match op {
                    blueberry_ast::UnaryOperator::Plus => quote!(+),
                    blueberry_ast::UnaryOperator::Minus => quote!(-),
                };
                quote!(( $op_token $inner ))
            }
            ConstValue::BinaryOp { op, left, right } => {
                let lhs = self.render_const_typed(left, scope, target_ty);
                let rhs = self.render_const_typed(right, scope, target_ty);
                let op_token = match op {
                    blueberry_ast::BinaryOperator::Add => quote!(+),
                    blueberry_ast::BinaryOperator::Subtract => quote!(-),
                    blueberry_ast::BinaryOperator::Multiply => quote!(*),
                    blueberry_ast::BinaryOperator::Divide => quote!(/),
                };
                quote!(( $lhs $op_token $rhs ))
            }
            _ => self.render_const(value, scope),
        }
    }

    fn render_const(&self, value: &ConstValue, scope: &[String]) -> Tokens {
        match value {
            ConstValue::Integer(lit) => {
                let literal = lit.value;
                quote!($literal)
            }
            ConstValue::Float(f) => {
                let mut literal = format!("{}", f);
                if !literal.contains('.') && !literal.contains('e') && !literal.contains('E') {
                    literal.push_str(".0");
                }
                quote!($literal)
            }
            ConstValue::Fixed(fixed) => {
                let mut literal = format!("{}", fixed.to_f64());
                if !literal.contains('.') && !literal.contains('e') && !literal.contains('E') {
                    literal.push_str(".0");
                }
                quote!($literal)
            }
            ConstValue::Binary(binary) => {
                let literal = binary.to_i128();
                quote!($literal)
            }
            ConstValue::String(text) => {
                quote!($(format!("\"{text}\"")))
            }
            ConstValue::Boolean(true) => quote!(true),
            ConstValue::Boolean(false) => quote!(false),
            ConstValue::Char(ch) => {
                let escaped: String = ch.escape_default().collect();
                quote!($(format!("'{}'", escaped)))
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

    fn relative_path(&self, target: &[String], scope: &[String]) -> Tokens {
        if let Some(resolved) = self.registry.resolve_struct(target, scope) {
            return self.build_crate_path(&self.flatten_path(&resolved));
        }
        if let Some(resolved) = self.registry.resolve_message(target, scope) {
            return self.build_crate_path(&self.flatten_path(&resolved));
        }
        if let Some(resolved) = self.registry.resolve_enum(target, scope) {
            return self.build_crate_path(&self.flatten_path(&resolved));
        }
        if let Some(resolved) = self.registry.resolve_typedef(target, scope) {
            return self.build_crate_path(&self.flatten_path(&resolved));
        }
        if target.len() >= 2 {
            let enum_name = &target[..target.len() - 1];
            let variant = &target[target.len() - 1];
            if let Some(resolved) = self.registry.resolve_enum(enum_name, scope) {
                let mut full = self.flatten_path(&resolved);
                full.push(variant.clone());
                return self.build_crate_path(&full);
            }
        }
        self.build_crate_path(&self.flatten_path(target))
    }

    fn build_crate_path(&self, segments: &[String]) -> Tokens {
        let mut path = String::from("crate::blueberry");
        for segment in segments {
            path.push_str("::");
            path.push_str(segment);
        }
        quote!($path)
    }
}

fn collect_definition_names(defs: &[Definition]) -> Vec<String> {
    let mut names = Vec::new();
    for def in defs {
        match def {
            Definition::ModuleDef(m) => {
                names.extend(collect_definition_names(&m.node.definitions));
            }
            Definition::TypeDef(d) => names.push(d.node.name.clone()),
            Definition::EnumDef(d) => names.push(d.node.name.clone()),
            Definition::StructDef(d) => names.push(d.node.name.clone()),
            Definition::MessageDef(d) => names.push(d.node.name.clone()),
            Definition::ConstDef(d) => names.push(d.node.name.clone()),
            Definition::ImportDef(_) => {}
        }
    }
    names
}

fn partition_into_file_modules(definitions: &[Definition]) -> Vec<FileModule> {
    let (defs, root_scope) = skip_wrapper_modules(definitions);

    let groups: Vec<&Commented<ModuleDef>> = defs
        .iter()
        .filter_map(|d| match d {
            Definition::ModuleDef(m) => Some(m),
            _ => None,
        })
        .collect();

    let has_non_modules = defs
        .iter()
        .any(|d| !matches!(d, Definition::ModuleDef(_) | Definition::ImportDef(_)));

    if has_non_modules || groups.is_empty() {
        return vec![FileModule {
            scope: root_scope,
            definitions: defs.to_vec(),
        }];
    }

    let mut result = Vec::new();

    for group in &groups {
        let mut group_scope = root_scope.clone();
        group_scope.push(group.node.name.clone());

        result.push(FileModule {
            scope: group_scope,
            definitions: group.node.definitions.clone(),
        });
    }

    result
}

fn skip_wrapper_modules(definitions: &[Definition]) -> (&[Definition], Vec<String>) {
    let mut current = definitions;
    let mut scope = Vec::new();

    loop {
        let mut module_count = 0;
        let mut single_module_defs: Option<&[Definition]> = None;
        let mut single_module_name: Option<String> = None;
        let mut has_non_modules = false;

        for d in current {
            match d {
                Definition::ModuleDef(m) => {
                    module_count += 1;
                    if module_count == 1 {
                        single_module_defs = Some(&m.node.definitions);
                        single_module_name = Some(m.node.name.clone());
                    }
                }
                Definition::ImportDef(_) => {}
                _ => {
                    has_non_modules = true;
                }
            }
        }

        if has_non_modules || module_count != 1 {
            break;
        }

        scope.push(single_module_name.unwrap());
        current = single_module_defs.unwrap();
    }

    (current, scope)
}

fn to_snake_case(name: &str) -> String {
    let mut result = String::new();
    let mut prev_was_lower = false;
    for ch in name.chars() {
        if ch.is_uppercase() {
            if prev_was_lower {
                result.push('_');
            }
            result.extend(ch.to_lowercase());
            prev_was_lower = false;
        } else {
            prev_was_lower = ch.is_lowercase();
            result.push(ch);
        }
    }
    result
}

fn scope_to_rust_file_path(scope: &[String]) -> String {
    let parts: Vec<String> = scope.iter().map(|s| to_snake_case(s)).collect();
    format!("rust/{}.rs", parts.join("/"))
}

fn scope_to_dir_path(scope: &[String]) -> String {
    let parts: Vec<String> = scope.iter().map(|s| to_snake_case(s)).collect();
    format!("rust/{}", parts.join("/"))
}

fn find_module_key_for_scope(definitions: &[Definition], scope: &[String]) -> Option<u16> {
    fn walk(
        definitions: &[Definition],
        scope: &[String],
        depth: usize,
        mk: Option<u16>,
    ) -> Option<u16> {
        if depth >= scope.len() {
            return mk;
        }
        for def in definitions {
            if let Definition::ModuleDef(module) = def
                && module.node.name == scope[depth]
            {
                let new_mk = annotation_u16(&module.annotations, "module_key").or(mk);
                return walk(&module.node.definitions, scope, depth + 1, new_mk);
            }
        }
        mk
    }
    walk(definitions, scope, 0, None)
}

fn annotation_value<'a>(annotations: &'a [Annotation], name: &str) -> Option<&'a ConstValue> {
    annotations
        .iter()
        .find(|a| {
            a.name
                .last()
                .map(|s| s.eq_ignore_ascii_case(name))
                .unwrap_or(false)
        })
        .and_then(|a| {
            a.params
                .iter()
                .map(|p| match p {
                    AnnotationParam::Named { name: n, value: v }
                        if n.eq_ignore_ascii_case("value") =>
                    {
                        v
                    }
                    AnnotationParam::Positional(v) => v,
                    AnnotationParam::Named { value: v, .. } => v,
                })
                .next()
        })
}

fn annotation_u16(annotations: &[Annotation], name: &str) -> Option<u16> {
    annotation_value(annotations, name).and_then(|v| match v {
        ConstValue::Integer(lit) if (0..=u16::MAX as i128).contains(&lit.value) => {
            Some(lit.value as u16)
        }
        _ => None,
    })
}

fn collect_message_info(
    definitions: &[Definition],
    scope: &[String],
    parent_module_key: Option<u16>,
) -> Vec<MessageInfo> {
    let mut result = Vec::new();

    for def in definitions {
        match def {
            Definition::ModuleDef(module) => {
                let mk = annotation_u16(&module.annotations, "module_key").or(parent_module_key);
                let mut child_scope = scope.to_vec();
                child_scope.push(module.node.name.clone());
                result.extend(collect_message_info(
                    &module.node.definitions,
                    &child_scope,
                    mk,
                ));
            }
            Definition::MessageDef(message_def) => {
                let explicit_msgk = annotation_u16(&message_def.annotations, "message_key");
                if explicit_msgk.is_none() {
                    continue;
                }
                let module_key = annotation_u16(&message_def.annotations, "module_key")
                    .or(parent_module_key)
                    .unwrap_or(DEFAULT_MODULE_KEY);
                let name = &message_def.node.name;
                let mut full_path = scope.to_vec();
                full_path.push(name.clone());
                result.push(MessageInfo {
                    variant_name: name.clone(),
                    rust_path: full_path,
                    module_key,
                    message_key: explicit_msgk.unwrap(),
                });
            }
            _ => {}
        }
    }

    result
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

    #[test]
    fn emits_topic_helpers_for_topic_annotations() {
        let definitions = load_fixture("crates/parser/tests/fixtures/message_default.idl");
        let files = generate_rust(&definitions).expect("generation should succeed");
        let all_content: String = files.iter().map(|f| f.contents.as_str()).collect();

        assert!(
            all_content.contains("TOPIC_TEMPLATE"),
            "generated code should include topic template"
        );
        assert!(
            all_content.contains("blueberry/devices/status"),
            "topic literal should be embedded"
        );
        assert!(
            !all_content.contains("declare_publisher"),
            "zenoh publisher helper should NOT be generated"
        );
        assert!(
            !all_content.contains("declare_subscriber"),
            "zenoh subscriber helper should NOT be generated"
        );
    }

    #[test]
    fn runtime_uses_blueberry_serde() {
        let generator = RustGenerator::new(&[]);
        let runtime = generator.generate_runtime();

        assert!(
            runtime.contains("format_topic"),
            "runtime should expose topic formatter"
        );
        assert!(
            runtime.contains("blueberry_serde"),
            "runtime should use blueberry_serde"
        );
        assert!(
            !runtime.contains("zenoh"),
            "runtime should not reference zenoh"
        );
        assert!(
            !runtime.contains("cdr::"),
            "runtime should not reference the cdr crate"
        );
    }
}
