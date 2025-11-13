use rust_lalrpop_experiment::{
    AnnotationParam, ConstValue, Definition, ImportScope, Type, TypeDef, parse_idl,
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

#[test]
fn parses_import_declarations() {
    let defs = parse_fixture("imports.idl");
    assert_eq!(defs.len(), 3);

    fn expect_import(def: &Definition) -> &rust_lalrpop_experiment::ImportDef {
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
        Some(ConstValue::Integer(value)) => assert_eq!(*value, 0),
        other => panic!("expected ACTIVE to have integer value, found {:?}", other),
    }

    let inactive = &enum_def.enumerators[1];
    assert_eq!(inactive.name, "INACTIVE");
    match &inactive.value {
        Some(ConstValue::Integer(value)) => assert_eq!(*value, 1),
        other => panic!("expected INACTIVE to have integer value, found {:?}", other),
    }

    let pending = &enum_def.enumerators[2];
    assert_eq!(pending.name, "PENDING");
    assert!(
        pending.value.is_none(),
        "PENDING should not assign an explicit value"
    );
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
    assert_eq!(struct_def.members.len(), 4);

    let name_member = &struct_def.members[0].node;
    assert_eq!(name_member.name, "name");
    assert!(matches!(name_member.type_, Type::String));

    let age_member = &struct_def.members[1].node;
    assert_eq!(age_member.name, "age");
    assert!(matches!(age_member.type_, Type::Long));

    let active_member = &struct_def.members[2].node;
    assert_eq!(active_member.name, "isActive");
    assert!(matches!(active_member.type_, Type::Boolean));

    let readings_member = &struct_def.members[3].node;
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
    assert_eq!(defs.len(), 4);

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

    match &expect_typedef(iter.next().unwrap()).base_type {
        Type::Sequence { element_type, size } => {
            assert!(matches!(**element_type, Type::Long));
            assert_eq!(size, &None);
        }
        other => panic!("expected sequence type, found {:?}", other),
    }

    match &expect_typedef(iter.next().unwrap()).base_type {
        Type::Sequence { element_type, size } => {
            assert!(matches!(**element_type, Type::String));
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
    assert!(matches!(max_clients.value, ConstValue::Integer(42)));

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
fn attaches_annotations_and_comments() {
    let defs = parse_fixture("annotations.idl");
    assert_eq!(defs.len(), 1);

    let struct_def = match &defs[0] {
        Definition::StructDef(def) => def,
        other => panic!("expected struct definition, found {:?}", other),
    };

    assert_eq!(
        struct_def.comments,
        vec!["// Tests annotation parsing and comment capture."]
    );
    assert_eq!(struct_def.annotations.len(), 1);

    let applied = &struct_def.annotations[0];
    assert_eq!(applied.name, vec!["Blueprint", "Experimental"]);
    assert_eq!(applied.params.len(), 1);
    match &applied.params[0] {
        AnnotationParam::Named { name, value } => {
            assert_eq!(name, "role");
            assert!(matches!(value, ConstValue::Integer(1)));
        }
    }

    let members = &struct_def.node.members;
    assert_eq!(members.len(), 2);

    assert_eq!(members[0].comments, vec!["// Primary identifier"]);
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
    }

    assert_eq!(members[1].annotations.len(), 1);
    let flag_ann = &members[1].annotations[0];
    assert_eq!(flag_ann.name, vec!["Wire", "Flag"]);
    assert!(flag_ann.params.is_empty());
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
