use std::collections::{BTreeMap, BTreeSet};

use blueberry_ast::{
    Annotation, AnnotationParam, Commented, ConstDef, ConstValue, Definition, EnumDef, MessageDef,
    ModuleDef, StructDef, Type, TypeDef,
};
use blueberry_codegen_core::{CodegenError, DEFAULT_MODULE_KEY, GeneratedFile, TypeRegistry};
use genco::lang::python::Tokens;
use genco::quote;

pub fn generate(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let generator = PythonGenerator::new(definitions);
    generator.generate_files(definitions)
}

struct FileModule {
    scope: Vec<String>,
    definitions: Vec<Definition>,
}

struct MessageInfo {
    class_name: String,
    python_module: String,
    module_key: u16,
    message_key: u16,
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

    fn generate_files(
        &self,
        definitions: &[Definition],
    ) -> Result<Vec<GeneratedFile>, CodegenError> {
        let file_modules = Self::partition_into_file_modules(definitions);

        let type_locations = self.build_type_locations(&file_modules);

        let mut generated = Vec::new();
        let mut package_dirs: BTreeSet<String> = BTreeSet::new();

        for fm in &file_modules {
            let file_path = scope_to_file_path(&fm.scope);

            let mut dir = std::path::Path::new(&file_path);
            while let Some(parent) = dir.parent() {
                let p = parent.to_string_lossy().to_string();
                if p.is_empty() {
                    break;
                }
                package_dirs.insert(p.clone());
                dir = parent;
            }

            let contents = self.render_file_module(fm, &type_locations)?;
            generated.push(GeneratedFile {
                path: file_path,
                contents,
            });
        }

        for dir in &package_dirs {
            generated.push(GeneratedFile {
                path: format!("{dir}/__init__.py"),
                contents: String::new(),
            });
        }

        let messages = self.collect_all_message_info(&file_modules);
        if !messages.is_empty() {
            generated.push(GeneratedFile {
                path: "python/message.py".to_string(),
                contents: self.emit_root_init(&messages),
            });
        }

        Ok(generated)
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
                scope: if root_scope.is_empty() {
                    vec!["messages".to_string()]
                } else {
                    root_scope
                },
                definitions: defs.to_vec(),
            }];
        }

        let mut result = Vec::new();

        for group in &groups {
            let mut group_scope = root_scope.clone();
            group_scope.push(group.node.name.clone());

            let child_modules: Vec<&Commented<ModuleDef>> = group
                .node
                .definitions
                .iter()
                .filter_map(|d| match d {
                    Definition::ModuleDef(m) => Some(m),
                    _ => None,
                })
                .collect();

            let group_has_defs = group
                .node
                .definitions
                .iter()
                .any(|d| !matches!(d, Definition::ModuleDef(_) | Definition::ImportDef(_)));

            if child_modules.is_empty() || group_has_defs {
                result.push(FileModule {
                    scope: group_scope,
                    definitions: group.node.definitions.clone(),
                });
            } else {
                for child in child_modules {
                    let mut child_scope = group_scope.clone();
                    child_scope.push(child.node.name.clone());
                    result.push(FileModule {
                        scope: child_scope,
                        definitions: child.node.definitions.clone(),
                    });
                }
            }
        }

        result
    }

    fn build_type_locations(
        &self,
        file_modules: &[FileModule],
    ) -> BTreeMap<Vec<String>, (Vec<String>, String)> {
        let mut map = BTreeMap::new();
        for fm in file_modules {
            Self::collect_type_locations(&fm.definitions, &fm.scope, &[], &mut map);
        }
        map
    }

    fn collect_type_locations(
        definitions: &[Definition],
        file_scope: &[String],
        inner_scope: &[String],
        map: &mut BTreeMap<Vec<String>, (Vec<String>, String)>,
    ) {
        for def in definitions {
            match def {
                Definition::ModuleDef(m) => {
                    let mut new_inner: Vec<String> = inner_scope.to_vec();
                    new_inner.push(m.node.name.clone());
                    Self::collect_type_locations(&m.node.definitions, file_scope, &new_inner, map);
                }
                Definition::EnumDef(e) => {
                    let mut full_path = file_scope.to_vec();
                    full_path.extend(inner_scope.iter().cloned());
                    full_path.push(e.node.name.clone());
                    map.insert(full_path, (file_scope.to_vec(), e.node.name.clone()));
                }
                Definition::StructDef(s) => {
                    let mut full_path = file_scope.to_vec();
                    full_path.extend(inner_scope.iter().cloned());
                    full_path.push(s.node.name.clone());
                    map.insert(full_path, (file_scope.to_vec(), s.node.name.clone()));
                }
                Definition::MessageDef(m) => {
                    let mut full_path = file_scope.to_vec();
                    full_path.extend(inner_scope.iter().cloned());
                    full_path.push(m.node.name.clone());
                    map.insert(full_path, (file_scope.to_vec(), m.node.name.clone()));
                }
                _ => {}
            }
        }
    }

    fn render_file_module(
        &self,
        fm: &FileModule,
        type_locations: &BTreeMap<Vec<String>, (Vec<String>, String)>,
    ) -> Result<String, CodegenError> {
        let mut imports_needed: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        let defs = self.emit_file_definitions(
            &fm.definitions,
            &fm.scope,
            &mut Vec::new(),
            None,
            type_locations,
            &mut imports_needed,
        )?;

        let wire_types = self.collect_wire_types_in(&fm.definitions, &fm.scope, &[]);
        let wire_type_imports = if wire_types.is_empty() {
            Tokens::new()
        } else {
            let joined = wire_types.join(", ");
            quote!(from blueberry_serde.types import $joined)
        };

        let cross_imports: Vec<Tokens> = imports_needed
            .iter()
            .flat_map(|(module, names)| {
                names.iter().map(move |name| {
                    let m = module.clone();
                    let n = name.clone();
                    quote!(from $m import $n)
                })
            })
            .collect();

        let tokens: Tokens = quote! {
            # Auto-generated Blueberry bindings

            from __future__ import annotations
            import enum
            from typing import Annotated, ClassVar

            from pydantic import BaseModel

            from blueberry_serde import (
                serialize,
                deserialize,
                serialize_message,
                deserialize_message,
                serialize_packet,
                deserialize_packet,
                empty_message,
            )
            $wire_type_imports
            $(for imp in &cross_imports => $imp$['\r'])

            $(for def in defs =>
                $def

            )
        };

        Ok(tokens.to_file_string().expect("render python output"))
    }

    fn emit_file_definitions(
        &self,
        defs: &[Definition],
        file_scope: &[String],
        inner_scope: &mut Vec<String>,
        parent_module_key: Option<u16>,
        type_locations: &BTreeMap<Vec<String>, (Vec<String>, String)>,
        imports_needed: &mut BTreeMap<String, BTreeSet<String>>,
    ) -> Result<Vec<Tokens>, CodegenError> {
        let mut out = Vec::new();
        let mut message_keys = MessageKeyGen::default();

        for def in defs {
            match def {
                Definition::ModuleDef(module) => {
                    let mk =
                        annotation_u16(&module.annotations, "module_key").or(parent_module_key);
                    inner_scope.push(module.node.name.clone());
                    let nested = self.emit_file_definitions(
                        &module.node.definitions,
                        file_scope,
                        inner_scope,
                        mk,
                        type_locations,
                        imports_needed,
                    )?;
                    out.extend(nested);
                    inner_scope.pop();
                }
                Definition::EnumDef(enum_def) => {
                    out.push(quote! { $['\n'] });
                    out.push(self.emit_enum_local(enum_def));
                }
                Definition::StructDef(struct_def) => {
                    out.push(quote! { $['\n'] });
                    out.push(self.emit_struct_local(
                        struct_def,
                        file_scope,
                        inner_scope,
                        type_locations,
                        imports_needed,
                    ));
                }
                Definition::MessageDef(message_def) => {
                    let module_key = annotation_u16(&message_def.annotations, "module_key")
                        .or(parent_module_key)
                        .unwrap_or(DEFAULT_MODULE_KEY);
                    let message_key = annotation_u16(&message_def.annotations, "message_key")
                        .unwrap_or_else(|| message_keys.next());
                    out.push(quote! { $['\n'] });
                    out.push(self.emit_message_local(
                        message_def,
                        file_scope,
                        inner_scope,
                        module_key,
                        message_key,
                        type_locations,
                        imports_needed,
                    )?);
                }
                Definition::ConstDef(const_def) => {
                    out.push(quote! { $['\r'] });
                    out.push(self.emit_const_local(const_def, inner_scope));
                }
                Definition::TypeDef(typedef_def) => {
                    out.push(quote! { $['\r'] });
                    out.push(self.emit_typedef_local(
                        typedef_def,
                        file_scope,
                        inner_scope,
                        type_locations,
                        imports_needed,
                    ));
                }
                Definition::ImportDef(_) => {}
            }
        }

        Ok(out)
    }

    fn emit_enum_local(&self, enum_def: &Commented<EnumDef>) -> Tokens {
        let name = enum_def.node.name.clone();
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
                    $m$['\r']
                )
        }
    }

    fn emit_struct_local(
        &self,
        struct_def: &Commented<StructDef>,
        file_scope: &[String],
        inner_scope: &[String],
        type_locations: &BTreeMap<Vec<String>, (Vec<String>, String)>,
        imports_needed: &mut BTreeMap<String, BTreeSet<String>>,
    ) -> Tokens {
        let mut full_path = file_scope.to_vec();
        full_path.extend(inner_scope.iter().cloned());
        full_path.push(struct_def.node.name.clone());
        let mut members = self.registry.collect_struct_members(&full_path);
        self.registry.sort_members_by_alignment(&mut members);
        let name = struct_def.node.name.clone();
        let fields: Tokens = quote! {
            $(for member in &members =>
                $(member.name.clone()): $(self.python_type_annotation_local(&member.ty, file_scope, inner_scope, type_locations, imports_needed))$['\r']
            )
        };

        quote! {
            class $name(BaseModel):
                $fields
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_message_local(
        &self,
        message_def: &Commented<MessageDef>,
        file_scope: &[String],
        inner_scope: &[String],
        module_key: u16,
        message_key: u16,
        type_locations: &BTreeMap<Vec<String>, (Vec<String>, String)>,
        imports_needed: &mut BTreeMap<String, BTreeSet<String>>,
    ) -> Result<Tokens, CodegenError> {
        let mut scope_for_lookup = file_scope.to_vec();
        scope_for_lookup.extend(inner_scope.iter().cloned());

        let topic = annotation_string(&message_def.annotations, "topic").ok_or(
            CodegenError::MissingTopic {
                message: scoped_name(&scope_for_lookup, &message_def.node.name),
            },
        )?;

        let mut full_path = scope_for_lookup.clone();
        full_path.push(message_def.node.name.clone());
        let mut members = self.registry.collect_message_members(&full_path);
        self.registry.sort_members_by_alignment(&mut members);
        let name = message_def.node.name.clone();
        let fields: Tokens = quote! {
            $(for member in &members =>
                $(member.name.clone()): $(self.python_type_annotation_local(&member.ty, file_scope, inner_scope, type_locations, imports_needed))$['\r']
            )
        };
        let topic_literal = quoted_string(&topic);

        Ok(quote! {
            class $name(BaseModel):
                topic_template: ClassVar[str] = $topic_literal
                module_key: ClassVar[int] = $module_key
                message_key: ClassVar[int] = $message_key

                $fields

                @classmethod
                def topic(cls, **kwargs: str) -> str:
                    return cls.topic_template.format(**kwargs)
        })
    }

    fn emit_const_local(&self, const_def: &Commented<ConstDef>, inner_scope: &[String]) -> Tokens {
        let name = local_name(inner_scope, &const_def.node.name);
        let value = const_literal(&const_def.node.value);
        quote!( $name = $value )
    }

    fn emit_typedef_local(
        &self,
        typedef_def: &Commented<TypeDef>,
        file_scope: &[String],
        inner_scope: &[String],
        type_locations: &BTreeMap<Vec<String>, (Vec<String>, String)>,
        imports_needed: &mut BTreeMap<String, BTreeSet<String>>,
    ) -> Tokens {
        let name = local_name(inner_scope, &typedef_def.node.name);
        let mut full_scope = file_scope.to_vec();
        full_scope.extend(inner_scope.iter().cloned());
        let base = self.python_type_hint_local(
            &typedef_def.node.base_type,
            file_scope,
            &full_scope,
            type_locations,
            imports_needed,
        );
        quote!( $name = $base )
    }

    fn python_type_annotation_local(
        &self,
        ty: &Type,
        file_scope: &[String],
        inner_scope: &[String],
        type_locations: &BTreeMap<Vec<String>, (Vec<String>, String)>,
        imports_needed: &mut BTreeMap<String, BTreeSet<String>>,
    ) -> Tokens {
        let mut full_scope = file_scope.to_vec();
        full_scope.extend(inner_scope.iter().cloned());
        let resolved = self.registry.resolve_type(ty, &full_scope);
        match &resolved {
            Type::Boolean => quote!(bool),
            Type::Char => quote!(str),
            Type::Octet => quote!(Annotated[int, UInt8]),
            Type::Short => quote!(Annotated[int, Int16]),
            Type::UnsignedShort => quote!(Annotated[int, UInt16]),
            Type::Long => quote!(Annotated[int, Int32]),
            Type::UnsignedLong => quote!(Annotated[int, UInt32]),
            Type::LongLong => quote!(Annotated[int, Int64]),
            Type::UnsignedLongLong => quote!(Annotated[int, UInt64]),
            Type::Float => quote!(Annotated[float, Float32]),
            Type::Double => quote!(float),
            Type::String { .. } => quote!(str),
            Type::Sequence { element_type, .. } => {
                let inner = self.python_type_annotation_local(
                    element_type,
                    file_scope,
                    inner_scope,
                    type_locations,
                    imports_needed,
                );
                quote!(list[$inner])
            }
            Type::Array {
                element_type,
                dimensions,
            } => {
                let mut elem = *element_type.clone();
                for _ in dimensions {
                    elem = Type::Sequence {
                        element_type: Box::new(elem),
                        size: None,
                    };
                }
                self.python_type_annotation_local(
                    &elem,
                    file_scope,
                    inner_scope,
                    type_locations,
                    imports_needed,
                )
            }
            Type::ScopedName(path) => {
                let name_tok =
                    self.resolve_scoped_ref(path, file_scope, type_locations, imports_needed);
                if let Some(base) = self.registry.enum_base(path) {
                    if let Some(wire) = wire_type_name(base) {
                        quote!(Annotated[$name_tok, $wire])
                    } else {
                        name_tok
                    }
                } else {
                    name_tok
                }
            }
            Type::WString | Type::WChar | Type::LongDouble => quote!(object),
        }
    }

    fn python_type_hint_local(
        &self,
        ty: &Type,
        file_scope: &[String],
        resolve_scope: &[String],
        type_locations: &BTreeMap<Vec<String>, (Vec<String>, String)>,
        imports_needed: &mut BTreeMap<String, BTreeSet<String>>,
    ) -> Tokens {
        let resolved = self.registry.resolve_type(ty, resolve_scope);
        match &resolved {
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
                let inner = self.python_type_hint_local(
                    element_type,
                    file_scope,
                    resolve_scope,
                    type_locations,
                    imports_needed,
                );
                quote!(list[$inner])
            }
            Type::Array {
                element_type,
                dimensions,
            } => {
                let mut elem = *element_type.clone();
                for _ in dimensions {
                    elem = Type::Sequence {
                        element_type: Box::new(elem),
                        size: None,
                    };
                }
                self.python_type_hint_local(
                    &elem,
                    file_scope,
                    resolve_scope,
                    type_locations,
                    imports_needed,
                )
            }
            Type::ScopedName(path) => {
                self.resolve_scoped_ref(path, file_scope, type_locations, imports_needed)
            }
            Type::WString | Type::WChar | Type::LongDouble => quote!(object),
        }
    }

    fn resolve_scoped_ref(
        &self,
        path: &[String],
        file_scope: &[String],
        type_locations: &BTreeMap<Vec<String>, (Vec<String>, String)>,
        imports_needed: &mut BTreeMap<String, BTreeSet<String>>,
    ) -> Tokens {
        if let Some((target_file_scope, local_name)) = type_locations.get(path) {
            if target_file_scope == file_scope {
                let ident = local_name.clone();
                quote!($ident)
            } else {
                let python_module = scope_to_python_module(target_file_scope);
                imports_needed
                    .entry(python_module)
                    .or_default()
                    .insert(local_name.clone());
                let ident = local_name.clone();
                quote!($ident)
            }
        } else {
            let ident = path.last().cloned().unwrap_or_default();
            quote!($ident)
        }
    }

    fn collect_wire_types_in(
        &self,
        defs: &[Definition],
        file_scope: &[String],
        inner_scope: &[String],
    ) -> Vec<String> {
        let mut types = BTreeSet::new();
        self.walk_wire_types_in(defs, file_scope, inner_scope, &mut types);
        types.into_iter().collect()
    }

    fn walk_wire_types_in(
        &self,
        defs: &[Definition],
        file_scope: &[String],
        inner_scope: &[String],
        out: &mut BTreeSet<String>,
    ) {
        for def in defs {
            match def {
                Definition::ModuleDef(module) => {
                    let mut new_inner = inner_scope.to_vec();
                    new_inner.push(module.node.name.clone());
                    self.walk_wire_types_in(&module.node.definitions, file_scope, &new_inner, out);
                }
                Definition::StructDef(struct_def) => {
                    let mut path = file_scope.to_vec();
                    path.extend(inner_scope.iter().cloned());
                    path.push(struct_def.node.name.clone());
                    let members = self.registry.collect_struct_members(&path);
                    for m in &members {
                        let mut scope = file_scope.to_vec();
                        scope.extend(inner_scope.iter().cloned());
                        self.collect_wire_type_for(&m.ty, &scope, out);
                    }
                }
                Definition::MessageDef(message_def) => {
                    let mut path = file_scope.to_vec();
                    path.extend(inner_scope.iter().cloned());
                    path.push(message_def.node.name.clone());
                    let members = self.registry.collect_message_members(&path);
                    for m in &members {
                        let mut scope = file_scope.to_vec();
                        scope.extend(inner_scope.iter().cloned());
                        self.collect_wire_type_for(&m.ty, &scope, out);
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_wire_type_for(&self, ty: &Type, scope: &[String], out: &mut BTreeSet<String>) {
        let resolved = self.registry.resolve_type(ty, scope);
        match &resolved {
            Type::Octet => {
                out.insert("UInt8".to_string());
            }
            Type::Short => {
                out.insert("Int16".to_string());
            }
            Type::UnsignedShort => {
                out.insert("UInt16".to_string());
            }
            Type::Long => {
                out.insert("Int32".to_string());
            }
            Type::UnsignedLong => {
                out.insert("UInt32".to_string());
            }
            Type::LongLong => {
                out.insert("Int64".to_string());
            }
            Type::UnsignedLongLong => {
                out.insert("UInt64".to_string());
            }
            Type::Float => {
                out.insert("Float32".to_string());
            }
            Type::Sequence { element_type, .. } => {
                self.collect_wire_type_for(element_type, scope, out);
            }
            Type::Array {
                element_type,
                dimensions,
            } => {
                let mut elem = *element_type.clone();
                for _ in dimensions {
                    elem = Type::Sequence {
                        element_type: Box::new(elem),
                        size: None,
                    };
                }
                self.collect_wire_type_for(&elem, scope, out);
            }
            Type::ScopedName(path) => {
                if let Some(base) = self.registry.enum_base(path)
                    && let Some(wire) = wire_type_name(base)
                {
                    out.insert(wire.to_string());
                }
            }
            _ => {}
        }
    }

    fn collect_all_message_info(&self, file_modules: &[FileModule]) -> Vec<MessageInfo> {
        let mut result = Vec::new();
        for fm in file_modules {
            self.collect_message_info_from(&fm.definitions, &fm.scope, None, &mut result);
        }
        result
    }

    fn collect_message_info_from(
        &self,
        definitions: &[Definition],
        file_scope: &[String],
        parent_module_key: Option<u16>,
        result: &mut Vec<MessageInfo>,
    ) {
        for def in definitions {
            match def {
                Definition::ModuleDef(module) => {
                    let mk =
                        annotation_u16(&module.annotations, "module_key").or(parent_module_key);
                    self.collect_message_info_from(
                        &module.node.definitions,
                        file_scope,
                        mk,
                        result,
                    );
                }
                Definition::MessageDef(message_def) => {
                    let explicit_msgk = annotation_u16(&message_def.annotations, "message_key");
                    if explicit_msgk.is_none() {
                        continue;
                    }
                    let module_key = annotation_u16(&message_def.annotations, "module_key")
                        .or(parent_module_key)
                        .unwrap_or(DEFAULT_MODULE_KEY);
                    let python_module = scope_to_python_module(file_scope);
                    result.push(MessageInfo {
                        class_name: message_def.node.name.clone(),
                        python_module,
                        module_key,
                        message_key: explicit_msgk.unwrap(),
                    });
                }
                _ => {}
            }
        }
    }

    fn emit_root_init(&self, messages: &[MessageInfo]) -> String {
        let mut imports = Vec::new();
        let mut registry_entries = Vec::new();
        let mut from_raw_arms = Vec::new();
        let mut display_arms = Vec::new();

        for msg in messages {
            imports.push(format!(
                "from {} import {}",
                msg.python_module, msg.class_name
            ));
            registry_entries.push(format!(
                "    (0x{:04X}, 0x{:04X}): {},",
                msg.module_key, msg.message_key, msg.class_name
            ));
            from_raw_arms.push(format!(
                "            (0x{:04X}, 0x{:04X}): {name},",
                msg.module_key,
                msg.message_key,
                name = msg.class_name
            ));
            display_arms.push(format!(
                "        if isinstance(self.inner, {name}):\n\
                 \x20           return \"{name}: \" + self.inner.model_dump_json(indent=2)",
                name = msg.class_name
            ));
        }

        let imports_block = imports.join("\n");
        let registry_block = registry_entries.join("\n");
        let _from_raw_block = from_raw_arms.join("\n");
        let display_block = display_arms.join("\n");

        let union_types = messages
            .iter()
            .map(|m| m.class_name.as_str())
            .collect::<Vec<_>>()
            .join(", ");

        let mut out = String::new();
        out.push_str("# Auto-generated Blueberry message registry\n");
        out.push_str("\nfrom __future__ import annotations\n");
        out.push_str("\nimport struct\n");
        out.push_str("from dataclasses import dataclass\n");
        out.push_str("from typing import Union\n");
        out.push_str("\nfrom blueberry_serde import (\n");
        out.push_str("    BLUEBERRY_PORT,\n");
        out.push_str("    HEADER_SIZE,\n");
        out.push_str("    PACKET_MAGIC,\n");
        out.push_str("    MessageHeader,\n");
        out.push_str("    deserialize_message,\n");
        out.push_str("    deserialize_packet,\n");
        out.push_str("    empty_message,\n");
        out.push_str("    serialize_packet,\n");
        out.push_str(")\n\n");
        out.push_str(&imports_block);
        out.push_str("\n\n_MESSAGE_REGISTRY: dict[tuple[int, int], type] = {\n");
        out.push_str(&registry_block);
        out.push_str("\n}\n");
        out.push_str("\n\n@dataclass\nclass Message:\n");
        out.push_str(&format!("    inner: Union[{union_types}]\n"));
        out.push_str("    module_key: int\n");
        out.push_str("    message_key: int\n");
        out.push_str(concat!(
            "\n    @staticmethod\n",
            "    def from_raw(data: bytes) -> \"Message\":\n",
            "        hdr = MessageHeader.decode(data)\n",
            "        if hdr is None:\n",
            "            raise ValueError(\"Invalid message header\")\n",
            "        key = (hdr.module_key, hdr.message_key)\n",
            "        model_type = _MESSAGE_REGISTRY.get(key)\n",
            "        if model_type is None:\n",
            "            raise ValueError(\n",
            "                f\"Unknown message: module=0x{hdr.module_key:04X} \"\n",
            "                f\"key=0x{hdr.message_key:04X}\"\n",
            "            )\n",
            "        _, fields = deserialize_message(data, model_type)\n",
            "        return Message(inner=fields, module_key=hdr.module_key, message_key=hdr.message_key)\n",
        ));
        out.push_str(concat!(
            "\n    def request_packet(self) -> bytes:\n",
            "        return serialize_packet([empty_message(self.module_key, self.message_key)])\n",
        ));
        out.push_str(concat!(
            "\n    @staticmethod\n",
            "    def request_packet_for(msg_cls: type) -> bytes:\n",
            "        return serialize_packet([empty_message(msg_cls.module_key, msg_cls.message_key)])\n",
        ));
        out.push_str(concat!(
            "\n    @staticmethod\n",
            "    def extract_packets(buf: bytearray) -> list[\"Message\"]:\n",
            "        results: list[\"Message\"] = []\n",
            "        while True:\n",
            "            idx = buf.find(PACKET_MAGIC)\n",
            "            if idx < 0:\n",
            "                break\n",
            "            if idx > 0:\n",
            "                buf[:idx] = b\"\"\n",
            "            if len(buf) < 8:\n",
            "                break\n",
            "            length_words = struct.unpack_from(\"<H\", buf, 4)[0]\n",
            "            packet_len = length_words * 4\n",
            "            if packet_len < 8 or len(buf) < packet_len:\n",
            "                break\n",
            "            try:\n",
            "                _pkt_hdr, raw_messages = deserialize_packet(bytes(buf[:packet_len]))\n",
            "                for raw_msg in raw_messages:\n",
            "                    try:\n",
            "                        results.append(Message.from_raw(raw_msg))\n",
            "                    except ValueError:\n",
            "                        pass\n",
            "                buf[:packet_len] = b\"\"\n",
            "            except ValueError:\n",
            "                buf[:4] = b\"\"\n",
            "        return results\n",
        ));
        out.push_str("\n    def __str__(self) -> str:\n");
        out.push_str(&display_block);
        out.push_str(concat!(
            "\n        return (\n",
            "            f\"Unknown(0x{self.module_key:04X}, 0x{self.message_key:04X})\"\n",
            "        )\n",
        ));
        out
    }
}

fn wire_type_name(ty: &Type) -> Option<&'static str> {
    match ty {
        Type::Octet => Some("UInt8"),
        Type::Short => Some("Int16"),
        Type::UnsignedShort => Some("UInt16"),
        Type::Long => Some("Int32"),
        Type::UnsignedLong => Some("UInt32"),
        Type::LongLong => Some("Int64"),
        Type::UnsignedLongLong => Some("UInt64"),
        Type::Float => Some("Float32"),
        _ => None,
    }
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

fn scope_to_file_path(scope: &[String]) -> String {
    let parts: Vec<String> = scope.iter().map(|s| to_snake_case(s)).collect();
    format!("python/{}.py", parts.join("/"))
}

fn scope_to_python_module(scope: &[String]) -> String {
    scope
        .iter()
        .map(|s| to_snake_case(s))
        .collect::<Vec<_>>()
        .join(".")
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

fn local_name(inner_scope: &[String], name: &str) -> String {
    if inner_scope.is_empty() {
        name.to_string()
    } else {
        let mut parts = inner_scope.to_vec();
        parts.push(name.to_string());
        parts.join("_")
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
