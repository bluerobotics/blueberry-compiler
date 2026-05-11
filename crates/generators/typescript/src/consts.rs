//! Emit `export const` declarations and `export type` aliases for IDL `const`/`typedef`.

use blueberry_ast::{
    BinaryOperator, Commented, ConstDef, ConstValue, TypeDef, UnaryOperator,
};

use crate::TypeScriptGenerator;

impl TypeScriptGenerator {
    pub(crate) fn emit_const(
        &self,
        const_def: &Commented<ConstDef>,
        _scope: &[String],
    ) -> String {
        let name = &const_def.node.name;
        let value = render_const_value(&const_def.node.value);
        format!("export const {name} = {value};\n")
    }

    pub(crate) fn emit_typedef(
        &self,
        typedef_def: &Commented<TypeDef>,
        scope: &[String],
    ) -> String {
        let name = &typedef_def.node.name;
        let ts_ty = self.render_ts_type(&typedef_def.node.base_type, scope);
        format!("export type {name} = {ts_ty};\n")
    }
}

fn render_const_value(value: &ConstValue) -> String {
    match value {
        ConstValue::Integer(lit) => format!("{}", lit.value),
        ConstValue::Float(v) => format_float_literal(*v),
        ConstValue::Fixed(fixed) => {
            let mut text = format!("{}", fixed.to_f64());
            if !text.contains('.') && !text.contains('e') && !text.contains('E') {
                text.push_str(".0");
            }
            text
        }
        ConstValue::Binary(bin) => format!("{}", bin.to_i128()),
        ConstValue::String(s) => quoted_ts_string(s),
        ConstValue::Boolean(true) => "true".to_string(),
        ConstValue::Boolean(false) => "false".to_string(),
        ConstValue::Char(ch) => quoted_ts_string(&ch.to_string()),
        ConstValue::ScopedName(path) => path.last().cloned().unwrap_or_else(|| "0".to_string()),
        ConstValue::UnaryOp { op, expr } => {
            let inner = render_const_value(expr);
            match op {
                UnaryOperator::Plus => format!("(+{inner})"),
                UnaryOperator::Minus => format!("(-{inner})"),
            }
        }
        ConstValue::BinaryOp { op, left, right } => {
            let lhs = render_const_value(left);
            let rhs = render_const_value(right);
            let op_str = match op {
                BinaryOperator::Add => "+",
                BinaryOperator::Subtract => "-",
                BinaryOperator::Multiply => "*",
                BinaryOperator::Divide => "/",
            };
            format!("({lhs} {op_str} {rhs})")
        }
    }
}

fn format_float_literal(value: f64) -> String {
    let mut text = format!("{value}");
    if !text.contains('.') && !text.contains('e') && !text.contains('E') {
        text.push_str(".0");
    }
    text
}

fn quoted_ts_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('\'');
    for ch in value.chars() {
        match ch {
            '\'' => out.push_str("\\'"),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out.push('\'');
    out
}
