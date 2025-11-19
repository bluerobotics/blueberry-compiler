use std::collections::HashMap;

use evalexpr::{Value, eval};
use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(grammar);

pub use ast::*;
pub use grammar::*;

const TOPIC_ANNOTATION_NAME: &str = "topic";

type MessageValidationFn =
    fn(scope: &[String], message_def: &Commented<MessageDef>) -> Result<(), String>;

/// Parse an IDL file string into a vector of definitions
pub fn parse_idl(input: &str) -> Result<Vec<Definition>, String> {
    let mut defs = grammar::IdlFileParser::new()
        .parse(input)
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Pos processing AST (Abstract Syntax Tree)
    fold_numeric_constants(&mut defs);
    propagate_enum_values(&mut defs);
    validate_message_annotations(&defs)?;

    Ok(defs)
}

fn fold_numeric_constants(defs: &mut [Definition]) {
    if defs.is_empty() {
        return;
    }

    let mut folder = NumericConstantFolder::default();
    folder.collect_constants(defs);
    folder.fold_all_constants();
    folder.apply(defs);
}

fn propagate_enum_values(defs: &mut [Definition]) {
    for def in defs {
        match def {
            Definition::EnumDef(enum_def) => assign_enum_members(&mut enum_def.node.enumerators),
            Definition::ModuleDef(module_def) => {
                propagate_enum_values(&mut module_def.node.definitions)
            }
            _ => {}
        }
    }
}

fn validate_message_annotations(defs: &[Definition]) -> Result<(), String> {
    let validators: &[MessageValidationFn] = &[ensure_topic_annotation];
    let mut scope = Vec::new();
    validate_messages_in_scope(defs, &mut scope, validators)
}

fn validate_messages_in_scope(
    defs: &[Definition],
    scope: &mut Vec<String>,
    validators: &[MessageValidationFn],
) -> Result<(), String> {
    for def in defs {
        match def {
            Definition::ModuleDef(module_def) => {
                scope.push(module_def.node.name.clone());
                validate_messages_in_scope(&module_def.node.definitions, scope, validators)?;
                scope.pop();
            }
            Definition::MessageDef(message_def) => {
                for validator in validators {
                    validator(scope, message_def)?;
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn ensure_topic_annotation(
    scope: &[String],
    message_def: &Commented<MessageDef>,
) -> Result<(), String> {
    if has_annotation(&message_def.annotations, TOPIC_ANNOTATION_NAME) {
        return Ok(());
    }

    let full_name = scoped_message_name(scope, &message_def.node.name);
    Err(format!(
        "message {full_name} must declare a @topic annotation"
    ))
}

fn has_annotation(annotations: &[Annotation], expected_name: &str) -> bool {
    annotations.iter().any(|annotation| {
        annotation
            .name
            .last()
            .map(|segment| segment.eq_ignore_ascii_case(expected_name))
            .unwrap_or(false)
    })
}

fn scoped_message_name(scope: &[String], message_name: &str) -> String {
    if message_name.is_empty() {
        return "<unnamed message>".to_string();
    }

    if scope.is_empty() {
        message_name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), message_name)
    }
}

#[derive(Default)]
struct NumericConstantFolder {
    constants: HashMap<Vec<String>, ConstEntry>,
}

impl NumericConstantFolder {
    fn collect_constants(&mut self, defs: &[Definition]) {
        let mut scope = Vec::new();
        self.collect_constants_in_scope(defs, &mut scope);
    }

    fn collect_constants_in_scope(&mut self, defs: &[Definition], scope: &mut Vec<String>) {
        for def in defs {
            match def {
                Definition::ModuleDef(module_def) => {
                    scope.push(module_def.node.name.clone());
                    self.collect_constants_in_scope(&module_def.node.definitions, scope);
                    scope.pop();
                }
                Definition::ConstDef(const_def) => {
                    let mut path = scope.clone();
                    path.push(const_def.node.name.clone());
                    self.constants.entry(path).or_insert_with(|| {
                        ConstEntry::new(scope.clone(), const_def.node.value.clone())
                    });
                }
                _ => {}
            }
        }
    }

    fn fold_all_constants(&mut self) {
        let keys: Vec<Vec<String>> = self.constants.keys().cloned().collect();
        for key in keys {
            self.resolve_constant(&key);
        }
    }

    fn resolve_constant(&mut self, path: &[String]) {
        match self.constants.get(path).map(|entry| entry.state) {
            Some(FoldState::Resolved) | None => return,
            Some(FoldState::Resolving) => return,
            Some(FoldState::Unresolved) => {}
        }

        if let Some(entry) = self.constants.get_mut(path) {
            entry.state = FoldState::Resolving;
        }

        let (value_snapshot, scope_snapshot) = match self.constants.get(path) {
            Some(entry) => (entry.value.clone(), entry.scope.clone()),
            None => return,
        };

        let folded_value = self.try_fold_numeric(&value_snapshot, &scope_snapshot, Some(path));

        if let Some(new_value) = folded_value
            && let Some(entry) = self.constants.get_mut(path)
        {
            entry.value = new_value;
        }

        if let Some(entry) = self.constants.get_mut(path) {
            entry.state = FoldState::Resolved;
        }
    }

    fn resolve_scoped_value(
        &mut self,
        scoped_name: &[String],
        scope: &[String],
        current_path: Option<&[String]>,
    ) -> Option<ConstValue> {
        let target_path = self.find_const_path(scoped_name, scope)?;
        if current_path
            .map(|path| path == target_path.as_slice())
            .unwrap_or(false)
        {
            return None;
        }

        self.resolve_constant(&target_path);
        let value = self.constants.get(&target_path)?.value.clone();
        if Self::is_numeric_literal(&value) {
            Some(value)
        } else {
            None
        }
    }

    fn try_fold_numeric(
        &mut self,
        value: &ConstValue,
        scope: &[String],
        current_path: Option<&[String]>,
    ) -> Option<ConstValue> {
        match value {
            ConstValue::Integer(_)
            | ConstValue::Float(_)
            | ConstValue::Fixed(_)
            | ConstValue::Binary(_) => Some(value.clone()),
            ConstValue::ScopedName(names) => self.resolve_scoped_value(names, scope, current_path),
            ConstValue::UnaryOp { .. } | ConstValue::BinaryOp { .. } => {
                let expression = self.build_numeric_expression(value, scope, current_path)?;
                Self::evaluate_numeric_expression(&expression)
            }
            _ => None,
        }
    }

    fn build_numeric_expression(
        &mut self,
        value: &ConstValue,
        scope: &[String],
        current_path: Option<&[String]>,
    ) -> Option<String> {
        match value {
            ConstValue::Integer(literal) => Some(literal.value.to_string()),
            ConstValue::Float(f) => Some(format_float_value(*f)),
            ConstValue::Fixed(fixed) => Some(format_float_value(fixed.to_f64())),
            ConstValue::Binary(literal) => Some(literal.to_i64().to_string()),
            ConstValue::ScopedName(names) => {
                let resolved = self.resolve_scoped_value(names, scope, current_path)?;
                self.build_numeric_expression(&resolved, scope, current_path)
            }
            ConstValue::UnaryOp { op, expr } => {
                let expr_str = self.build_numeric_expression(expr, scope, current_path)?;
                let op_str = match op {
                    UnaryOperator::Plus => "+",
                    UnaryOperator::Minus => "-",
                };
                Some(format!("({op_str}{expr_str})"))
            }
            ConstValue::BinaryOp { op, left, right } => {
                let left_expr = self.build_numeric_expression(left, scope, current_path)?;
                let right_expr = self.build_numeric_expression(right, scope, current_path)?;
                let op_str = match op {
                    BinaryOperator::Add => "+",
                    BinaryOperator::Subtract => "-",
                    BinaryOperator::Multiply => "*",
                    BinaryOperator::Divide => "/",
                };
                Some(format!("({left_expr} {op_str} {right_expr})"))
            }
            _ => None,
        }
    }

    fn evaluate_numeric_expression(expression: &str) -> Option<ConstValue> {
        let value = eval(expression).ok()?;
        match value {
            Value::Int(v) => Some(ConstValue::Integer(IntegerLiteral::new(
                v,
                IntegerBase::Decimal,
            ))),
            Value::Float(v) => Some(ConstValue::Float(v)),
            _ => None,
        }
    }

    fn find_const_path(&self, scoped_name: &[String], scope: &[String]) -> Option<Vec<String>> {
        if scoped_name.is_empty() {
            return None;
        }

        for prefix in (0..=scope.len()).rev() {
            let mut candidate: Vec<String> = scope[..prefix].to_vec();
            candidate.extend_from_slice(scoped_name);
            if self.constants.contains_key(&candidate) {
                return Some(candidate);
            }
        }

        None
    }

    fn apply(&mut self, defs: &mut [Definition]) {
        let mut scope = Vec::new();
        self.apply_in_scope(defs, &mut scope);
    }

    fn apply_in_scope(&mut self, defs: &mut [Definition], scope: &mut Vec<String>) {
        for def in defs {
            match def {
                Definition::ModuleDef(module_def) => {
                    self.fold_annotations(&mut module_def.annotations, scope);
                    scope.push(module_def.node.name.clone());
                    self.apply_in_scope(&mut module_def.node.definitions, scope);
                    scope.pop();
                }
                Definition::EnumDef(enum_def) => {
                    self.fold_annotations(&mut enum_def.annotations, scope);
                    for member in &mut enum_def.node.enumerators {
                        if let Some(value) = &mut member.value {
                            self.fold_const_value(value, scope);
                        }
                    }
                }
                Definition::StructDef(struct_def) => {
                    self.fold_annotations(&mut struct_def.annotations, scope);
                    for member in &mut struct_def.node.members {
                        self.fold_annotations(&mut member.annotations, scope);
                    }
                }
                Definition::MessageDef(message_def) => {
                    self.fold_annotations(&mut message_def.annotations, scope);
                    for member in &mut message_def.node.members {
                        self.fold_annotations(&mut member.annotations, scope);
                    }
                }
                Definition::ConstDef(const_def) => {
                    self.fold_annotations(&mut const_def.annotations, scope);
                    let mut path = scope.clone();
                    path.push(const_def.node.name.clone());
                    if let Some(entry) = self.constants.get(&path) {
                        const_def.node.value = entry.value.clone();
                    } else {
                        self.fold_const_value(&mut const_def.node.value, scope);
                    }
                }
                Definition::TypeDef(type_def) => {
                    self.fold_annotations(&mut type_def.annotations, scope);
                }
                Definition::ImportDef(import_def) => {
                    self.fold_annotations(&mut import_def.annotations, scope);
                }
            }
        }
    }

    fn fold_annotations(&mut self, annotations: &mut [Annotation], scope: &[String]) {
        for annotation in annotations {
            for param in &mut annotation.params {
                match param {
                    AnnotationParam::Named { value, .. } => self.fold_const_value(value, scope),
                    AnnotationParam::Positional(value) => self.fold_const_value(value, scope),
                }
            }
        }
    }

    fn fold_const_value(&mut self, value: &mut ConstValue, scope: &[String]) {
        match value {
            ConstValue::ScopedName(names) => {
                let scoped = names.clone();
                if let Some(resolved) = self.resolve_scoped_value(&scoped, scope, None) {
                    *value = resolved;
                }
            }
            ConstValue::UnaryOp { expr, .. } => {
                self.fold_const_value(expr, scope);
                let snapshot = value.clone();
                if let Some(resolved) = self.try_fold_numeric(&snapshot, scope, None) {
                    *value = resolved;
                }
            }
            ConstValue::BinaryOp { left, right, .. } => {
                self.fold_const_value(left, scope);
                self.fold_const_value(right, scope);
                let snapshot = value.clone();
                if let Some(resolved) = self.try_fold_numeric(&snapshot, scope, None) {
                    *value = resolved;
                }
            }
            _ => {}
        }
    }

    fn is_numeric_literal(value: &ConstValue) -> bool {
        matches!(
            value,
            ConstValue::Integer(_)
                | ConstValue::Float(_)
                | ConstValue::Fixed(_)
                | ConstValue::Binary(_)
        )
    }
}

#[derive(Clone)]
struct ConstEntry {
    scope: Vec<String>,
    value: ConstValue,
    state: FoldState,
}

impl ConstEntry {
    fn new(scope: Vec<String>, value: ConstValue) -> Self {
        ConstEntry {
            scope,
            value,
            state: FoldState::Unresolved,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum FoldState {
    Unresolved,
    Resolving,
    Resolved,
}

fn format_float_value(value: f64) -> String {
    let mut rendered = value.to_string();
    if !rendered.contains('.') && !rendered.contains('e') && !rendered.contains('E') {
        rendered.push_str(".0");
    }
    rendered
}

fn assign_enum_members(members: &mut [EnumMember]) {
    let mut next_value = Some(IntegerLiteral::new(0, IntegerBase::Decimal));
    for member in members.iter_mut() {
        match &member.value {
            Some(ConstValue::Integer(integer_literal)) => {
                let mut new_value = integer_literal.clone();
                new_value.value = new_value.value.checked_add(1).unwrap();
                next_value = Some(new_value);
            }
            Some(ConstValue::Binary(binary_literal)) => {
                let mut new_value =
                    IntegerLiteral::new(binary_literal.to_i64(), IntegerBase::Decimal);
                new_value.value = new_value.value.checked_add(1).unwrap();
                next_value = Some(new_value);
            }
            Some(_) => {
                next_value = None;
            }
            None => {
                if let Some(value) = next_value {
                    member.value = Some(ConstValue::Integer(value.clone()));
                    let mut new_value = value.clone();
                    new_value.value = new_value.value.checked_add(1).unwrap();
                    next_value = Some(new_value);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn parses_example_fixture() {
        let defs = load_fixture("example.idl");
        assert!(
            !defs.is_empty(),
            "example fixture should yield at least one definition"
        );
    }

    fn load_fixture(name: &str) -> Vec<Definition> {
        let path = format!("tests/fixtures/{name}");
        let input = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read fixture {}: {}", path, err));
        parse_idl(&input).expect("fixture should parse successfully")
    }
}
