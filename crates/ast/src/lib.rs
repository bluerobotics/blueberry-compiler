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
    MessageDef(Commented<MessageDef>),
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
    pub base: Option<Vec<String>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MessageDef {
    pub name: String,
    pub members: Vec<Commented<Member>>,
    pub base: Option<Vec<String>>,
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
    Fixed(FixedPointLiteral),
    String(String),
    Boolean(bool),
    Char(char),
    ScopedName(Vec<String>),
    UnaryOp {
        op: UnaryOperator,
        expr: Box<ConstValue>,
    },
    BinaryOp {
        op: BinaryOperator,
        left: Box<ConstValue>,
        right: Box<ConstValue>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FixedPointLiteral {
    /// Combined integer and fractional digits without a decimal point.
    pub digits: String,
    /// Number of digits that originally appeared after the decimal point.
    pub scale: u32,
    /// Whether the literal was prefixed with a minus sign.
    pub negative: bool,
}

impl FixedPointLiteral {
    pub fn from_literal_str(raw: &str) -> Self {
        let (negative, rest) = split_sign(raw);
        let magnitude = rest
            .strip_suffix('d')
            .or_else(|| rest.strip_suffix('D'))
            .expect("fixed-point literal must end with d or D");

        let mut digits = String::new();
        let mut scale = 0_u32;

        if let Some(dot_idx) = magnitude.find('.') {
            let integer = &magnitude[..dot_idx];
            let fraction = &magnitude[dot_idx + 1..];
            digits.push_str(integer);
            digits.push_str(fraction);
            scale = fraction.len() as u32;
        } else {
            digits.push_str(magnitude);
        }

        assert!(
            !digits.is_empty(),
            "fixed-point literal must include at least one digit"
        );

        FixedPointLiteral {
            digits,
            scale,
            negative,
        }
    }

    pub fn precision(&self) -> usize {
        self.digits.len()
    }

    pub fn to_f64(&self) -> f64 {
        let mut value = 0_f64;
        for ch in self.digits.chars() {
            value = value * 10.0 + ch.to_digit(10).unwrap_or(0) as f64;
        }
        if self.scale > 0 {
            value /= 10_f64.powi(self.scale as i32);
        }
        if self.negative { -value } else { value }
    }
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
/// Supports both named parameters (`@MyAnn(foo = 1)`) and positional ones
/// (`@MyAnn("param")`). An empty parameter list yields the bare form `@MyAnn`.
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationParam {
    Named { name: String, value: ConstValue },
    Positional(ConstValue),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
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
    String {
        bound: Option<u32>,
    },
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
