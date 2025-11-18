#![cfg(target_arch = "wasm32")]

use blueberry_idl_generator::generate_idl;
use blueberry_parser::parse_idl;
use js_sys::{Object, Reflect};
use wasm_bindgen::{JsValue, prelude::*};

#[wasm_bindgen(start)]
pub fn wasm_start() {
    console_error_panic_hook::set_once();
}

/// Parse the given IDL source and return either the formatted AST
/// or the parse error description.
#[wasm_bindgen]
pub fn parse_idl_wasm(input: &str) -> String {
    match parse_idl(input) {
        Ok(defs) => format!("{:#?}", defs),
        Err(err) => err,
    }
}

/// Parse and reserialize the given IDL, returning a JS object with both results.
#[wasm_bindgen]
pub fn analyze_idl_wasm(input: &str) -> JsValue {
    let result = Object::new();
    match parse_idl(input) {
        Ok(defs) => {
            let ast = format!("{:#?}", defs);
            let generated = generate_idl(&defs);
            Reflect::set(&result, &"ast".into(), &JsValue::from_str(&ast)).unwrap();
            Reflect::set(&result, &"generated".into(), &JsValue::from_str(&generated)).unwrap();
            Reflect::set(&result, &"error".into(), &JsValue::UNDEFINED).unwrap();
        }
        Err(err) => {
            let err_str = JsValue::from_str(&err);
            Reflect::set(&result, &"ast".into(), &err_str).unwrap();
            let generated_msg = format!("Cannot generate IDL:\n{err}");
            Reflect::set(
                &result,
                &"generated".into(),
                &JsValue::from_str(&generated_msg),
            )
            .unwrap();
            Reflect::set(&result, &"error".into(), &JsValue::from_str(&err)).unwrap();
        }
    }
    result.into()
}
