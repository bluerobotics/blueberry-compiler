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
    ImportDef(Commented<ImportDef>),
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
    pub base_type: Option<Type>,
    pub enumerators: Vec<EnumMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMember {
    pub name: String,
    pub value: Option<ConstValue>,
    pub comments: Vec<String>,
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
pub struct ImportDef {
    pub scope: ImportScope,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportScope {
    Scoped(Vec<String>),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Integer(IntegerLiteral),
    Float(f64),
    String(String),
    Boolean(bool),
    Char(char),
    ScopedName(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
    pub base: IntegerBase,
}

impl IntegerLiteral {
    pub fn new(value: i64, base: IntegerBase) -> Self {
        IntegerLiteral { value, base }
    }

    pub fn from_decimal_str(raw: &str) -> Self {
        let value = raw.parse::<i64>().unwrap();
        IntegerLiteral::new(value, IntegerBase::Decimal)
    }

    pub fn from_octal_str(raw: &str) -> Self {
        let (is_negative, digits) = split_sign(raw);
        let magnitude = i64::from_str_radix(digits, 8).unwrap();
        let value = if is_negative { -magnitude } else { magnitude };
        IntegerLiteral::new(value, IntegerBase::Octal)
    }

    pub fn from_hex_str(raw: &str) -> Self {
        let (is_negative, digits) = split_sign(raw);
        let digits = digits
            .strip_prefix("0x")
            .or_else(|| digits.strip_prefix("0X"))
            .expect("hex literal must start with 0x or 0X");
        let magnitude = i64::from_str_radix(digits, 16).unwrap();
        let value = if is_negative { -magnitude } else { magnitude };
        IntegerLiteral::new(value, IntegerBase::Hexadecimal)
    }
}

fn split_sign(raw: &str) -> (bool, &str) {
    if let Some(rest) = raw.strip_prefix('-') {
        (true, rest)
    } else {
        (false, raw)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntegerBase {
    Decimal,
    Octal,
    Hexadecimal,
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
