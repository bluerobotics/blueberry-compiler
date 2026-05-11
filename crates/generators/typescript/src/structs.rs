//! Emit `export interface` declarations for IDL `struct`s.

use blueberry_ast::{Commented, StructDef};

use crate::TypeScriptGenerator;
use crate::naming::camel_case;

impl TypeScriptGenerator {
    pub(crate) fn emit_struct(
        &self,
        struct_def: &Commented<StructDef>,
        scope: &[String],
    ) -> String {
        let name = &struct_def.node.name;
        let mut path = scope.to_vec();
        path.push(struct_def.node.name.clone());
        let mut members = self.registry.collect_struct_members(&path);
        self.registry.sort_members_by_alignment(&mut members);

        let mut out = String::new();
        out.push_str(&format!("export interface {name} {{\n"));
        for member in &members {
            let ty = self.render_ts_type(&member.ty, &path);
            out.push_str(&format!("  {}: {};\n", camel_case(&member.name), ty));
        }
        out.push_str("}\n");
        out
    }
}
