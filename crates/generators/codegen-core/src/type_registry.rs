use std::collections::{HashMap, HashSet};

use blueberry_ast::{Definition, MessageDef, StructDef, Type};

#[derive(Clone)]
pub struct TypedefInfo {
    pub ty: Type,
    pub scope: Vec<String>,
}

#[derive(Clone)]
pub struct StructInfo {
    pub def: StructDef,
    pub scope: Vec<String>,
}

#[derive(Clone)]
pub struct MessageInfo {
    pub def: MessageDef,
    pub scope: Vec<String>,
}

#[derive(Clone)]
pub struct ResolvedMember {
    pub name: String,
    pub ty: Type,
    pub comments: Vec<String>,
}

#[derive(Default)]
pub struct TypeRegistry {
    typedefs: HashMap<Vec<String>, TypedefInfo>,
    structs: HashMap<Vec<String>, StructInfo>,
    messages: HashMap<Vec<String>, MessageInfo>,
    enums: HashMap<Vec<String>, Type>,
}

impl TypeRegistry {
    pub fn new(definitions: &[Definition]) -> Self {
        let mut registry = TypeRegistry::default();
        let mut scope = Vec::new();
        registry.collect(definitions, &mut scope);
        registry
    }

    pub fn resolve_type(&self, ty: &Type, scope: &[String]) -> Type {
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

    pub fn collect_struct_members(&self, path: &[String]) -> Vec<ResolvedMember> {
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

    pub fn collect_message_members(&self, path: &[String]) -> Vec<ResolvedMember> {
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

    pub fn struct_paths(&self) -> Vec<Vec<String>> {
        self.structs.keys().cloned().collect()
    }

    pub fn enum_base(&self, path: &[String]) -> Option<&Type> {
        self.enums.get(path)
    }

    pub fn resolve_typedef(&self, name: &[String], scope: &[String]) -> Option<Vec<String>> {
        self.resolve_path(name, scope, self.typedefs.keys())
    }

    pub fn resolve_struct(&self, name: &[String], scope: &[String]) -> Option<Vec<String>> {
        self.resolve_path(name, scope, self.structs.keys())
    }

    pub fn resolve_message(&self, name: &[String], scope: &[String]) -> Option<Vec<String>> {
        self.resolve_path(name, scope, self.messages.keys())
    }

    pub fn resolve_enum(&self, name: &[String], scope: &[String]) -> Option<Vec<String>> {
        self.resolve_path(name, scope, self.enums.keys())
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
        let lookup: HashSet<Vec<String>> = paths.iter().cloned().collect();
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

pub fn map_builtin_ident(name: &str) -> Option<Type> {
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
