#![cfg(target_arch = "wasm32")]

use wasm_bindgen::prelude::*;

use crate::parse_idl;

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
