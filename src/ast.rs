/// Generic wrapper for AST nodes that can carry comments and annotations
#[derive(Debug, Clone, PartialEq)]
pub struct Commented<T> {
    pub comments: Vec<String>,
    pub annotations: Vec<Annotation>,
    pub node: T,
}

impl<T> Commented<T> {
    pub fn new(node: T, comments: Vec<String>) -> Self {
        Commented {
            comments,
            annotations: Vec::new(),
            node,
        }
    }

    pub fn with_annotations(node: T, comments: Vec<String>, annotations: Vec<Annotation>) -> Self {
        Commented {
            comments,
            annotations,
            node,
        }
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

/// Trait for uniform access to annotations
pub trait HasAnnotations {
    fn annotations(&self) -> &[Annotation];
    fn annotations_mut(&mut self) -> &mut Vec<Annotation>;
}

impl<T> HasAnnotations for Commented<T> {
    fn annotations(&self) -> &[Annotation] {
        &self.annotations
    }

    fn annotations_mut(&mut self) -> &mut Vec<Annotation> {
        &mut self.annotations
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

/// Applied annotation (e.g. @foo::bar(a = 1, b = "x"))
#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    /// The annotation name as a scoped identifier
    pub name: Vec<String>,
    /// Parameters to the annotation
    pub params: Vec<AnnotationParam>,
}

/// Parameters to an applied annotation.
///
/// For now, only the "named" form is supported:
///   @MyAnn(foo = 1, bar = TRUE)
/// and the empty form:
///   @MyAnn
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationParam {
    Named { name: String, value: ConstValue },
    // Positional(ConstValue),  // Can be added later if you want the shortened form
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
