/// Generic wrapper for AST nodes that can carry comments
#[derive(Debug, Clone, PartialEq)]
pub struct Commented<T> {
    pub comments: Vec<String>,
    pub node: T,
}

impl<T> Commented<T> {
    pub fn new(node: T, comments: Vec<String>) -> Self {
        Commented { comments, node }
    }
}

/// Trait for uniform access to comments
pub trait HasComments {
    fn comments(&self) -> &[String];
    fn comments_mut(&mut self) -> &mut Vec<String>;
}

impl<T> HasComments for Commented<T> {
    fn comments(&self) -> &[String] {
        &self.comments
    }

    fn comments_mut(&mut self) -> &mut Vec<String> {
        &mut self.comments
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    TypeDef(Commented<TypeDef>),
    EnumDef(Commented<EnumDef>),
    StructDef(Commented<StructDef>),
    ModuleDef(Commented<ModuleDef>),
    ConstDef(Commented<ConstDef>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDef {
    pub name: String,
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub base_type: Type,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub enumerators: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub members: Vec<Commented<Member>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub type_: Type,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDef {
    pub const_type: Type,
    pub name: String,
    pub value: ConstValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Char(char),
    ScopedName(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Basic types
    Long,
    Short,
    UnsignedLong,
    UnsignedShort,
    LongLong,
    UnsignedLongLong,
    Float,
    Double,
    LongDouble,
    Boolean,
    Char,
    WChar,
    Octet,
    String,
    WString,
    // Sequence type
    Sequence {
        element_type: Box<Type>,
        size: Option<u32>,
    },
    // Array type
    Array {
        element_type: Box<Type>,
        dimensions: Vec<u32>,
    },
    // User-defined types
    ScopedName(Vec<String>),
}
