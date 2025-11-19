use blueberry_parser::{
    AnnotationParam, Commented, ConstDef, ConstValue, Definition, FixedPointLiteral, ImportScope,
    IntegerBase, IntegerLiteral, Type, TypeDef, parse_idl,
};
use std::{fs, path::Path};

fn parse_fixture(name: &str) -> Vec<Definition> {
    let path = Path::new("tests/fixtures").join(name);
    let input = fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read fixture {}: {}", path.display(), err));
    parse_idl(&input).unwrap_or_else(|err| panic!("failed to parse {}: {}", name, err))
}

fn parse_example() -> Vec<Definition> {
    let input =
        fs::read_to_string("tests/fixtures/example.idl").expect("example.idl fixture should exist");
    parse_idl(&input).expect("example.idl fixture should parse")
}

fn scoped(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|s| s.to_string()).collect()
}

fn expect_const_value<'a>(defs: &'a [Definition], name: &str) -> &'a ConstValue {
    defs.iter()
        .find_map(|def| match def {
            Definition::ConstDef(const_def) if const_def.node.name == name => {
                Some(&const_def.node.value)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing const {}", name))
}

fn expect_scoped_const_def<'a>(defs: &'a [Definition], path: &[&str]) -> &'a Commented<ConstDef> {
    fn walk<'a>(defs: &'a [Definition], path: &[&str]) -> Option<&'a Commented<ConstDef>> {
        if path.is_empty() {
            return None;
        }
        if path.len() == 1 {
            let name = path[0];
            return defs.iter().find_map(|def| match def {
                Definition::ConstDef(c) if c.node.name == name => Some(c),
                _ => None,
            });
        }
        let (first, rest) = path.split_first().unwrap();
        let module_defs = defs.iter().find_map(|def| match def {
            Definition::ModuleDef(module) if module.node.name == *first => {
                Some(&module.node.definitions)
            }
            _ => None,
        })?;
        walk(module_defs, rest)
    }

    walk(defs, path).unwrap_or_else(|| panic!("missing const {:?}", path))
}

fn expect_scoped_const_value<'a>(defs: &'a [Definition], path: &[&str]) -> &'a ConstValue {
    &expect_scoped_const_def(defs, path).node.value
}

fn expect_integer(value: &ConstValue, expected: i64) -> &IntegerLiteral {
    match value {
        ConstValue::Integer(literal) => {
            assert_eq!(
                literal.value, expected,
                "expected integer value {}, found {}",
                expected, literal.value
            );
            literal
        }
        other => panic!("expected integer const {}, found {:?}", expected, other),
    }
}

fn expect_fixed(value: &ConstValue) -> &FixedPointLiteral {
    match value {
        ConstValue::Fixed(literal) => literal,
        other => panic!("expected fixed-point const, found {:?}", other),
    }
}

#[test]
fn parses_import_declarations() {
    let defs = parse_fixture("imports.idl");
    assert_eq!(defs.len(), 3);

    fn expect_import(def: &Definition) -> &blueberry_parser::ImportDef {
        match def {
            Definition::ImportDef(i) => &i.node,
            other => panic!("expected import declaration, found {:?}", other),
        }
    }

    match &expect_import(&defs[0]).scope {
        ImportScope::Scoped(path) => assert_eq!(path, &scoped(&["Example", "Utilities"])),
        other => panic!("expected scoped import path, found {:?}", other),
    }

    match &expect_import(&defs[1]).scope {
        ImportScope::Scoped(path) => assert_eq!(path, &scoped(&["Root", "Systems", "Diagnostics"])),
        other => panic!("expected absolute scoped import path, found {:?}", other),
    }

    match &expect_import(&defs[2]).scope {
        ImportScope::String(path) => assert_eq!(path, "external/sensors.idl"),
        other => panic!("expected string import path, found {:?}", other),
    }
}

#[test]
fn parses_module_definitions() {
    let defs = parse_fixture("module_only.idl");
    assert_eq!(defs.len(), 1);

    let module = match &defs[0] {
        Definition::ModuleDef(module) => &module.node,
        other => panic!("expected module definition, found {:?}", other),
    };
    assert_eq!(module.name, "Navigation");
    assert_eq!(module.definitions.len(), 1);

    match &module.definitions[0] {
        Definition::TypeDef(type_def) => {
            assert_eq!(type_def.node.name, "Heading");
            assert!(matches!(type_def.node.base_type, Type::Long));
        }
        other => panic!("expected nested typedef, found {:?}", other),
    }
}

#[test]
fn parses_enum_definitions() {
    let defs = parse_fixture("enum_only.idl");
    assert_eq!(defs.len(), 1);

    let enum_def = match &defs[0] {
        Definition::EnumDef(def) => &def.node,
        other => panic!("expected enum definition, found {:?}", other),
    };
    assert_eq!(enum_def.name, "Status");
    assert!(matches!(enum_def.base_type, Some(Type::UnsignedShort),));
    assert_eq!(enum_def.enumerators.len(), 3);

    let active = &enum_def.enumerators[0];
    assert_eq!(active.name, "ACTIVE");
    match &active.value {
        Some(value) => {
            expect_integer(value, 0);
        }
        other => panic!("expected ACTIVE to have integer value, found {:?}", other),
    }

    let inactive = &enum_def.enumerators[1];
    assert_eq!(inactive.name, "INACTIVE");
    match &inactive.value {
        Some(value) => {
            expect_integer(value, 1);
        }
        other => panic!("expected INACTIVE to have integer value, found {:?}", other),
    }

    let pending = &enum_def.enumerators[2];
    assert_eq!(pending.name, "PENDING");
    match &pending.value {
        Some(ConstValue::Integer(il)) => assert_eq!(il.value, 2),
        other => panic!(
            "expected PENDING to inherit integer value, found {:?}",
            other
        ),
    }
}

#[test]
fn infers_missing_enum_values() {
    let defs = parse_fixture("enum_auto_values.idl");
    assert_eq!(defs.len(), 1);

    let enum_def = match &defs[0] {
        Definition::EnumDef(def) => &def.node,
        other => panic!("expected enum definition, found {:?}", other),
    };

    let expected = [
        ("FIRST", 0),
        ("SECOND", 1),
        ("THIRD", 10),
        ("FOURTH", 11),
        ("FIFTH", 12),
    ];

    assert_eq!(enum_def.enumerators.len(), expected.len());
    for (member, (name, value)) in enum_def.enumerators.iter().zip(expected) {
        assert_eq!(&member.name, name);
        match &member.value {
            Some(ConstValue::Integer(actual)) => assert_eq!(actual.value, value),
            other => panic!(
                "expected {} to resolve to integer value {}, found {:?}",
                name, value, other
            ),
        }
    }
}

#[test]
fn parses_struct_members_and_arrays() {
    let defs = parse_fixture("struct_basic.idl");
    assert_eq!(defs.len(), 1);

    let struct_def = match &defs[0] {
        Definition::StructDef(def) => &def.node,
        other => panic!("expected struct definition, found {:?}", other),
    };

    assert_eq!(struct_def.name, "Person");
    assert_eq!(struct_def.members.len(), 5);

    let name_member = &struct_def.members[0].node;
    assert_eq!(name_member.name, "name");
    assert!(matches!(name_member.type_, Type::String { bound: None }));

    let age_member = &struct_def.members[1].node;
    assert_eq!(age_member.name, "age");
    assert!(matches!(age_member.type_, Type::Long));

    let active_member = &struct_def.members[2].node;
    assert_eq!(active_member.name, "isActive");
    assert!(matches!(active_member.type_, Type::Boolean));

    let codename_member = &struct_def.members[3].node;
    assert_eq!(codename_member.name, "codename");
    assert!(matches!(
        codename_member.type_,
        Type::String { bound: Some(16) }
    ));

    let readings_member = &struct_def.members[4].node;
    assert_eq!(readings_member.name, "readings");
    match &readings_member.type_ {
        Type::Array {
            element_type,
            dimensions,
        } => {
            assert_eq!(dimensions, &vec![8]);
            assert!(matches!(**element_type, Type::Long));
        }
        other => panic!("expected array type, found {:?}", other),
    }
}

#[test]
fn parses_typedefs_sequences_and_scoped_names() {
    let defs = parse_fixture("typedefs.idl");
    assert_eq!(defs.len(), 7);

    let mut iter = defs.iter();

    fn expect_typedef(def: &Definition) -> &TypeDef {
        match def {
            Definition::TypeDef(t) => &t.node,
            other => panic!("expected typedef, found {:?}", other),
        }
    }

    assert!(matches!(
        expect_typedef(iter.next().unwrap()).base_type,
        Type::Long
    ));

    assert!(matches!(
        expect_typedef(iter.next().unwrap()).base_type,
        Type::String { bound: Some(32) }
    ));

    match &expect_typedef(iter.next().unwrap()).base_type {
        Type::Sequence { element_type, size } => {
            assert!(matches!(**element_type, Type::Long));
            assert_eq!(size, &None);
        }
        other => panic!("expected sequence type, found {:?}", other),
    }

    match &expect_typedef(iter.next().unwrap()).base_type {
        Type::Sequence { element_type, size } => {
            assert!(matches!(**element_type, Type::String { bound: None }));
            assert_eq!(size, &Some(64));
        }
        other => panic!("expected bounded sequence type, found {:?}", other),
    }

    match &expect_typedef(iter.next().unwrap()).base_type {
        Type::ScopedName(names) => {
            assert_eq!(names, &scoped(&["Example", "Person"]));
        }
        other => panic!("expected scoped name typedef, found {:?}", other),
    }

    match &expect_typedef(iter.next().unwrap()).base_type {
        Type::Array {
            element_type,
            dimensions,
        } => {
            assert!(matches!(**element_type, Type::Octet));
            assert_eq!(dimensions, &vec![32]);
        }
        other => panic!("expected 1D array typedef, found {:?}", other),
    }

    match &expect_typedef(iter.next().unwrap()).base_type {
        Type::Array {
            element_type,
            dimensions,
        } => {
            assert!(matches!(**element_type, Type::Octet));
            assert_eq!(dimensions, &vec![3, 3]);
        }
        other => panic!("expected 2D array typedef, found {:?}", other),
    }
}

#[test]
fn parses_constant_values() {
    let defs = parse_fixture("consts.idl");
    assert_eq!(defs.len(), 7);

    let mut consts = defs.iter().filter_map(|def| {
        if let Definition::ConstDef(c) = def {
            Some(&c.node)
        } else {
            None
        }
    });

    let max_clients = consts.next().expect("MAX_CLIENTS const missing");
    assert_eq!(max_clients.name, "MAX_CLIENTS");
    expect_integer(&max_clients.value, 42);

    let pi_const = consts.next().expect("PI const missing");
    assert!(matches!(
        pi_const.value,
        ConstValue::Float(value) if (value - std::f64::consts::PI).abs() < f64::EPSILON
    ));

    let feature_flag = consts.next().expect("FEATURE_FLAG const missing");
    assert!(matches!(feature_flag.value, ConstValue::Boolean(true)));

    let newline_const = consts.next().expect("NEWLINE const missing");
    assert!(matches!(newline_const.value, ConstValue::Char('\n')));

    let product_const = consts.next().expect("PRODUCT const missing");
    assert!(matches!(product_const.value, ConstValue::String(ref s) if s == "Blueberry"));

    let default_status = consts.next().expect("DEFAULT_STATUS const missing");
    match &default_status.value {
        ConstValue::ScopedName(path) => assert_eq!(path, &scoped(&["Status", "ACTIVE"])),
        other => panic!("expected scoped const value, found {:?}", other),
    }
}

#[test]
fn parses_integer_literal_bases() {
    let defs = parse_fixture("integer_literals.idl");
    assert_eq!(defs.len(), 6);

    let decimal = expect_const_value(&defs, "DECIMAL_TWELVE");
    assert!(matches!(
        expect_integer(decimal, 12).base,
        IntegerBase::Decimal
    ));

    let octal = expect_const_value(&defs, "OCTAL_TWELVE");
    assert!(matches!(expect_integer(octal, 12).base, IntegerBase::Octal));

    let uppercase_hex = expect_const_value(&defs, "HEX_TWELVE");
    assert!(matches!(
        expect_integer(uppercase_hex, 12).base,
        IntegerBase::Hexadecimal
    ));

    let lower_hex = expect_const_value(&defs, "LOWER_HEX");
    assert!(matches!(
        expect_integer(lower_hex, 26).base,
        IntegerBase::Hexadecimal
    ));

    let negative_octal = expect_const_value(&defs, "NEGATIVE_OCTAL");
    assert!(matches!(
        expect_integer(negative_octal, -15).base,
        IntegerBase::Octal
    ));

    let negative_hex = expect_const_value(&defs, "NEGATIVE_HEX");
    assert!(matches!(
        expect_integer(negative_hex, -42).base,
        IntegerBase::Hexadecimal
    ));
}

#[test]
fn parses_floating_point_literals() {
    let defs = parse_fixture("floating_literals.idl");
    assert_eq!(defs.len(), 7);

    fn expect_float(value: &ConstValue) -> f64 {
        match value {
            ConstValue::Float(v) => *v,
            other => panic!("expected float const, found {:?}", other),
        }
    }

    fn assert_close(actual: f64, expected: f64) {
        let diff = (actual - expected).abs();
        let scale = expected.abs().max(1.0);
        let tolerance = f64::EPSILON * scale * 4.0;
        assert!(
            diff <= tolerance,
            "expected float {} within {} but got {}",
            expected,
            tolerance,
            actual
        );
    }

    assert_close(
        expect_float(expect_const_value(&defs, "FRACTION_ONLY")),
        0.625,
    );
    assert_close(
        expect_float(expect_const_value(&defs, "INTEGER_WITH_TRAILING_DOT")),
        42.0,
    );
    assert_close(expect_float(expect_const_value(&defs, "EXP_ONLY")), 7000.0);
    assert_close(
        expect_float(expect_const_value(&defs, "FRACTION_WITH_EXP")),
        5.0,
    );
    assert_close(
        expect_float(expect_const_value(&defs, "DECIMAL_WITH_NEG_EXP")),
        1.2,
    );
    assert_close(expect_float(expect_const_value(&defs, "UPPER_EXP")), 25.0);
    assert_close(
        expect_float(expect_const_value(&defs, "NEGATIVE_FRACTION")),
        -0.125,
    );
}

#[test]
fn parses_fixed_point_literals() {
    let defs = parse_fixture("fixed_literals.idl");
    assert_eq!(defs.len(), 5);

    let fractional = expect_fixed(expect_const_value(&defs, "FRACTIONAL"));
    assert_eq!(fractional.digits, "1234");
    assert_eq!(fractional.scale, 2);
    assert_eq!(fractional.precision(), 4);
    assert!(!fractional.negative);

    let fraction_only = expect_fixed(expect_const_value(&defs, "FRACTION_ONLY"));
    assert_eq!(fraction_only.digits, "625");
    assert_eq!(fraction_only.scale, 3);
    assert_eq!(fraction_only.precision(), 3);
    assert!(!fraction_only.negative);

    let integer_only = expect_fixed(expect_const_value(&defs, "INTEGER_ONLY"));
    assert_eq!(integer_only.digits, "42");
    assert_eq!(integer_only.scale, 0);
    assert_eq!(integer_only.precision(), 2);

    let trailing_dot = expect_fixed(expect_const_value(&defs, "TRAILING_DOT"));
    assert_eq!(trailing_dot.digits, "7");
    assert_eq!(trailing_dot.scale, 0);
    assert_eq!(trailing_dot.precision(), 1);
    assert!(!trailing_dot.negative);

    let negative_fraction = expect_fixed(expect_const_value(&defs, "NEGATIVE_FRACTION"));
    assert_eq!(negative_fraction.digits, "0875");
    assert_eq!(negative_fraction.scale, 3);
    assert_eq!(negative_fraction.precision(), 4);
    assert!(negative_fraction.negative);
}

#[test]
fn folds_numeric_constant_references() {
    fn assert_close(actual: f64, expected: f64) {
        let diff = (actual - expected).abs();
        let scale = expected.abs().max(1.0);
        let tolerance = f64::EPSILON * scale * 4.0;
        assert!(
            diff <= tolerance,
            "expected float {} within {} but got {}",
            expected,
            tolerance,
            actual
        );
    }

    let defs = parse_fixture("const_folding.idl");

    let expect_value = |path: &[&str]| expect_scoped_const_value(&defs, path);

    expect_integer(expect_value(&["GLOBAL_ALIAS"]), 12);
    expect_integer(expect_value(&["FORWARD_ALIAS"]), 64);
    expect_integer(expect_value(&["Outer", "MODULE_BASE"]), 21);
    expect_integer(expect_value(&["Outer", "MODULE_ALIAS"]), 21);
    expect_integer(expect_value(&["Outer", "MODULE_COMPLEX"]), 18);
    expect_integer(expect_value(&["Outer", "MODULE_MAGIC"]), 39);
    expect_integer(expect_value(&["Outer", "Inner", "FROM_PARENT"]), 21);
    expect_integer(expect_value(&["Outer", "Inner", "FROM_OUTER_SCOPED"]), 21);
    expect_integer(expect_value(&["Outer", "Inner", "FROM_ABSOLUTE"]), 21);

    let module_ratio = expect_value(&["Outer", "MODULE_RATIO"]);
    let module_ratio_value = match module_ratio {
        ConstValue::Float(v) => *v,
        other => panic!("expected MODULE_RATIO float, found {:?}", other),
    };
    assert_close(module_ratio_value, 2.75);

    let tau_alias = expect_value(&["TAU_ALIAS"]);
    let tau_value = match tau_alias {
        ConstValue::Float(v) => *v,
        other => panic!("expected TAU_ALIAS float, found {:?}", other),
    };
    assert_close(tau_value, std::f64::consts::PI);

    let fixed_alias = expect_value(&["FIXED_ALIAS"]);
    let fixed = expect_fixed(fixed_alias);
    assert_eq!(fixed.digits, "125");
    assert_eq!(fixed.scale, 1);

    let product_alias = expect_value(&["PRODUCT_ALIAS"]);
    assert!(matches!(
        product_alias,
        ConstValue::ScopedName(path) if path == &scoped(&["PRODUCT"])
    ));

    let annotated_const = expect_scoped_const_def(&defs, &["ANNOTATED_LIMIT"]);
    let const_annotation_value = annotated_const
        .annotations
        .first()
        .and_then(|ann| ann.params.first())
        .map(|param| match param {
            AnnotationParam::Named { value, .. } | AnnotationParam::Positional(value) => value,
        })
        .expect("ANNOTATED_LIMIT annotation missing");
    expect_integer(const_annotation_value, 12);

    let struct_def = defs
        .iter()
        .find_map(|def| match def {
            Definition::StructDef(defn) if defn.node.name == "AnnotatedStruct" => Some(defn),
            _ => None,
        })
        .expect("AnnotatedStruct struct missing");
    let struct_annotation_value = struct_def
        .annotations
        .first()
        .and_then(|ann| ann.params.first())
        .map(|param| match param {
            AnnotationParam::Named { value, .. } | AnnotationParam::Positional(value) => value,
        })
        .expect("struct annotation missing");
    expect_integer(struct_annotation_value, 12);

    let member = struct_def
        .node
        .members
        .first()
        .expect("threshold member missing");
    let member_annotation_value = member
        .annotations
        .first()
        .and_then(|ann| ann.params.first())
        .map(|param| match param {
            AnnotationParam::Named { value, .. } | AnnotationParam::Positional(value) => value,
        })
        .expect("member annotation missing");
    expect_integer(member_annotation_value, 12);

    let enum_def = defs
        .iter()
        .find_map(|def| match def {
            Definition::EnumDef(defn) if defn.node.name == "WithConst" => Some(&defn.node),
            _ => None,
        })
        .expect("WithConst enum missing");

    let start = enum_def
        .enumerators
        .iter()
        .find(|member| member.name == "START")
        .expect("START enumerator missing");
    expect_integer(
        start
            .value
            .as_ref()
            .expect("START enumerator missing value"),
        12,
    );

    let next = enum_def
        .enumerators
        .iter()
        .find(|member| member.name == "NEXT")
        .expect("NEXT enumerator missing");
    expect_integer(
        next.value.as_ref().expect("NEXT enumerator missing value"),
        13,
    );
}

#[test]
fn attaches_annotations_and_comments() {
    let defs = parse_fixture("annotations.idl");
    assert_eq!(defs.len(), 1);

    let struct_def = match &defs[0] {
        Definition::StructDef(def) => def,
        other => panic!("expected struct definition, found {:?}", other),
    };

    assert_eq!(
        struct_def.comments,
        vec!["Tests annotation parsing and comment capture."]
    );
    assert_eq!(struct_def.annotations.len(), 2);

    let marker = &struct_def.annotations[0];
    assert_eq!(marker.name, vec!["Blueprint", "Marker"]);
    assert!(marker.params.is_empty());

    let applied = &struct_def.annotations[1];
    assert_eq!(applied.name, vec!["Blueprint", "Experimental"]);
    assert_eq!(applied.params.len(), 1);
    match &applied.params[0] {
        AnnotationParam::Named { name, value } => {
            assert_eq!(name, "role");
            expect_integer(value, 1);
        }
        AnnotationParam::Positional(_) => {
            panic!("expected named struct annotation parameter");
        }
    }

    let members = &struct_def.node.members;
    assert_eq!(members.len(), 3);

    assert_eq!(members[0].comments, vec!["Primary identifier"]);
    assert_eq!(members[0].annotations.len(), 1);
    let member_ann = &members[0].annotations[0];
    assert_eq!(member_ann.name, vec!["Wire", "Encoded"]);
    match &member_ann.params[0] {
        AnnotationParam::Named { name, value } => {
            assert_eq!(name, "format");
            match value {
                ConstValue::ScopedName(path) => assert_eq!(path, &scoped(&["Status", "ACTIVE"])),
                other => panic!("expected scoped annotation value, found {:?}", other),
            }
        }
        AnnotationParam::Positional(_) => {
            panic!("expected named annotation parameter for encoded member");
        }
    }

    assert_eq!(members[1].comments, vec!["Display label"]);
    assert_eq!(members[1].annotations.len(), 1);
    let label_ann = &members[1].annotations[0];
    assert_eq!(label_ann.name, vec!["Wire", "Label"]);
    match &label_ann.params[0] {
        AnnotationParam::Positional(value) => match value {
            ConstValue::String(text) => assert_eq!(text, "primary"),
            other => panic!("expected string literal, found {:?}", other),
        },
        AnnotationParam::Named { .. } => {
            panic!("expected positional annotation parameter for label");
        }
    }

    assert_eq!(members[2].comments, vec!["Toggle flag"]);
    assert_eq!(members[2].annotations.len(), 2);

    let flag_ann = &members[2].annotations[0];
    assert_eq!(flag_ann.name, vec!["Wire", "Flag"]);
    assert!(flag_ann.params.is_empty());

    let flag_options = &members[2].annotations[1];
    assert_eq!(flag_options.name, vec!["Wire", "FlagOptions"]);
    assert_eq!(flag_options.params.len(), 2);
    match &flag_options.params[0] {
        AnnotationParam::Named { name, value } => {
            assert_eq!(name, "mode");
            match value {
                ConstValue::String(text) => assert_eq!(text, "test"),
                other => panic!("expected string literal, found {:?}", other),
            }
        }
        AnnotationParam::Positional(_) => panic!("expected named parameter for mode"),
    }
    match &flag_options.params[1] {
        AnnotationParam::Named { name, value } => {
            assert_eq!(name, "retries");
            expect_integer(value, 10);
        }
        AnnotationParam::Positional(_) => panic!("expected named parameter for retries"),
    }
}

#[test]
fn parses_message_definitions() {
    let defs = parse_fixture("message_basic.idl");
    assert_eq!(defs.len(), 1);

    let message = match &defs[0] {
        Definition::MessageDef(def) => def,
        other => panic!("expected message definition, found {:?}", other),
    };

    assert_eq!(message.node.name, "StatusUpdate");
    assert_eq!(message.annotations.len(), 2);
    assert_eq!(message.annotations[0].name, scoped(&["topic"]));
    assert_eq!(message.annotations[1].name, scoped(&["message_key"]));

    match &message.annotations[0].params[0] {
        AnnotationParam::Named { name, value } => {
            assert_eq!(name, "value");
            match value {
                ConstValue::String(path) => assert_eq!(path, "blueberry/devices/status"),
                other => panic!("expected string path, found {:?}", other),
            }
        }
        AnnotationParam::Positional(value) => {
            panic!("unexpected positional param {:?}", value);
        }
    }

    match &message.annotations[1].params[0] {
        AnnotationParam::Named { name, value } => {
            assert_eq!(name, "value");
            let literal = expect_integer(value, 0x47);
            assert_eq!(literal.base, IntegerBase::Hexadecimal);
        }
        AnnotationParam::Positional(value) => {
            panic!("unexpected positional param {:?}", value);
        }
    }

    let members = &message.node.members;
    assert_eq!(members.len(), 2);

    let id_member = &members[0];
    assert_eq!(id_member.node.name, "id");
    assert!(matches!(id_member.node.type_, Type::Long));

    let active_member = &members[1];
    assert_eq!(active_member.node.name, "active");
    assert!(matches!(active_member.node.type_, Type::Boolean));
}

#[test]
fn parses_struct_and_message_inheritance() {
    let defs = parse_fixture("inheritance.idl");
    assert_eq!(defs.len(), 4);

    let derived_struct = defs.iter().find_map(|def| match def {
        Definition::StructDef(struct_def) if struct_def.node.name == "DerivedStruct" => {
            Some(struct_def)
        }
        _ => None,
    });
    let derived_struct = derived_struct.expect("fixture should include DerivedStruct");
    assert_eq!(derived_struct.node.base, Some(scoped(&["BaseStruct"])));
    assert!(
        derived_struct
            .node
            .members
            .iter()
            .any(|member| member.node.name == "active"),
        "derived struct should retain its members"
    );

    let derived_message = defs.iter().find_map(|def| match def {
        Definition::MessageDef(message_def) if message_def.node.name == "DerivedMessage" => {
            Some(message_def)
        }
        _ => None,
    });
    let derived_message = derived_message.expect("fixture should include DerivedMessage");
    assert_eq!(derived_message.node.base, Some(scoped(&["BaseMessage"])));
    assert_eq!(derived_message.annotations.len(), 1);
    assert_eq!(derived_message.annotations[0].name, scoped(&["topic"]));
}

#[test]
fn rejects_multiple_struct_inheritance() {
    let src = r#"
struct BaseA {};
struct BaseB {};
struct Derived : BaseA, BaseB {
    long id;
};
"#;
    let err = parse_idl(src).expect_err("struct inheritance should reject multiple bases");
    assert!(
        err.contains("Parse error"),
        "unexpected error message: {err}"
    );
}

#[test]
fn parses_message_with_only_topic_annotation() {
    let src = r#"
@topic(value = "blueberry/devices/minimal")
message Minimal {
    long id;
};
"#;

    let defs = parse_idl(src).expect("message with topic annotation should parse");
    assert_eq!(defs.len(), 1);
    assert!(matches!(defs[0], Definition::MessageDef(_)));
}

#[test]
fn rejects_message_missing_topic_annotation() {
    let src = r#"
message Incomplete {
    long id;
};
"#;
    let err = parse_idl(src).expect_err("message without annotations should fail");
    assert!(err.contains("@topic"), "unexpected error message: {err}");
}

#[test]
fn rejects_message_missing_topic_even_with_other_annotations() {
    let src = r#"
@message_key(value = 0x1)
message Overflowing {
    long id;
};
"#;
    let err = parse_idl(src).expect_err("message without @topic should fail");
    assert!(err.contains("@topic"), "unexpected error message: {err}");
}

#[test]
fn parses_example_idl_end_to_end() {
    let defs = parse_example();
    assert_eq!(
        defs.len(),
        1,
        "example.idl currently exposes a single module"
    );

    let module = match &defs[0] {
        Definition::ModuleDef(def) => &def.node,
        other => panic!("expected module definition, found {:?}", other),
    };
    assert_eq!(module.name, "Example");

    assert!(
        module.definitions.len() >= 7,
        "example.idl should expose typedefs, enum, struct, and sequences"
    );

    let mut typedefs = 0;
    let mut enums = 0;
    let mut structs = 0;

    for def in &module.definitions {
        match def {
            Definition::TypeDef(_) => typedefs += 1,
            Definition::EnumDef(_) => enums += 1,
            Definition::StructDef(_) => structs += 1,
            _ => {}
        }
    }

    assert!(
        typedefs >= 5,
        "integration file should export several typedefs"
    );
    assert_eq!(
        enums, 2,
        "integration file should include the Status and HwType enums"
    );
    assert_eq!(
        structs, 1,
        "integration file should include the Person struct"
    );
}

#[test]
fn parses_scoped_module_names() {
    let defs = parse_fixture("scoped_modules.idl");
    assert_eq!(defs.len(), 1, "should parse one top-level module");

    // Verify the structure: module A { module B { module C { ... } } }
    let module_a = match &defs[0] {
        Definition::ModuleDef(module) => &module.node,
        other => panic!("expected module definition, found {:?}", other),
    };
    assert_eq!(module_a.name, "A", "outermost module should be named A");
    assert_eq!(
        module_a.definitions.len(),
        1,
        "module A should contain one definition"
    );

    let module_b = match &module_a.definitions[0] {
        Definition::ModuleDef(module) => &module.node,
        other => panic!("expected module definition in A, found {:?}", other),
    };
    assert_eq!(module_b.name, "B", "middle module should be named B");
    assert_eq!(
        module_b.definitions.len(),
        1,
        "module B should contain one definition"
    );

    let module_c = match &module_b.definitions[0] {
        Definition::ModuleDef(module) => &module.node,
        other => panic!("expected module definition in B, found {:?}", other),
    };
    assert_eq!(module_c.name, "C", "innermost module should be named C");
    assert_eq!(
        module_c.definitions.len(),
        2,
        "module C should contain two definitions"
    );

    // Verify the inner definitions
    match &module_c.definitions[0] {
        Definition::TypeDef(typedef) => {
            assert_eq!(typedef.node.name, "MyLong");
        }
        other => panic!("expected typedef, found {:?}", other),
    }

    match &module_c.definitions[1] {
        Definition::EnumDef(enum_def) => {
            assert_eq!(enum_def.node.name, "Status");
        }
        other => panic!("expected enum, found {:?}", other),
    }
}
