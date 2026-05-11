//! Emit per-message `<Name>Fields` interface + `<Name>Message` class with
//! `MODULE_KEY` / `MESSAGE_KEY` / `TOPIC_TEMPLATE` statics and explicit
//! encode/decode bodies built from `BlueberryWriter` / `BlueberryReader` calls.

use std::collections::BTreeSet;
use std::fmt::Write as _;

use blueberry_ast::{Commented, MessageDef};
use blueberry_codegen_core::CodegenError;

use crate::naming::{camel_case, pascal_case, quoted_string};
use crate::{
    MessageEntry, TypeScriptGenerator, annotation_string, emit_read_for_member,
    emit_write_for_member, scoped_name,
};

impl TypeScriptGenerator {
    pub(crate) fn emit_message(
        &self,
        message_def: &Commented<MessageDef>,
        scope: &[String],
        module_key: u16,
        message_key: u16,
        body: &mut String,
        _imports: &mut BTreeSet<&'static str>,
    ) -> Result<Option<MessageEntry>, CodegenError> {
        let topic = annotation_string(&message_def.annotations, "topic").ok_or(
            CodegenError::MissingTopic {
                message: scoped_name(scope, &message_def.node.name),
            },
        )?;

        let mut path = scope.to_vec();
        path.push(message_def.node.name.clone());
        let mut members = self.registry.collect_message_members(&path);
        self.registry.sort_members_by_alignment(&mut members);

        let base_name = pascal_case(&message_def.node.name);
        let fields_name = format!("{}Fields", base_name.trim_end_matches("Message"));
        let class_name = if base_name.ends_with("Message") {
            base_name.clone()
        } else {
            format!("{base_name}Message")
        };
        let topic_const_name = format!(
            "{}_TOPIC",
            class_name
                .replace("Message", "")
                .chars()
                .enumerate()
                .map(|(i, ch)| {
                    if i > 0 && ch.is_ascii_uppercase() {
                        format!("_{}", ch.to_ascii_uppercase())
                    } else {
                        ch.to_ascii_uppercase().to_string()
                    }
                })
                .collect::<String>(),
        );

        let _ = writeln!(
            body,
            "export const {const_name} = {literal};\n",
            const_name = topic_const_name,
            literal = quoted_string(&topic),
        );

        body.push_str(&format!("export interface {fields_name} {{\n"));
        for member in &members {
            let ts_ty = self.render_ts_type(&member.ty, &path);
            body.push_str(&format!("  {}: {};\n", camel_case(&member.name), ts_ty,));
        }
        body.push_str("}\n\n");

        body.push_str(&format!("export class {class_name} {{\n"));
        body.push_str(&format!(
            "  static readonly MODULE_KEY = 0x{:04x};\n",
            module_key,
        ));
        body.push_str(&format!(
            "  static readonly MESSAGE_KEY = 0x{:04x};\n",
            message_key,
        ));
        body.push_str(&format!(
            "  static readonly TOPIC_TEMPLATE = {topic_const};\n\n",
            topic_const = topic_const_name,
        ));

        let topic_params = collect_topic_params(&topic);
        if topic_params.is_empty() {
            body.push_str("  static topic(): string {\n");
            body.push_str(&format!(
                "    return {class}.TOPIC_TEMPLATE;\n",
                class = class_name,
            ));
            body.push_str("  }\n\n");
        } else {
            let params_ts = topic_params
                .iter()
                .map(|name| format!("{}: string", camel_case(name)))
                .collect::<Vec<_>>()
                .join("; ");
            body.push_str(&format!(
                "  static topic(params: {{ {params_ts} }}): string {{\n",
            ));
            body.push_str(&format!(
                "    let out = {class}.TOPIC_TEMPLATE as string;\n",
                class = class_name,
            ));
            for name in &topic_params {
                body.push_str(&format!(
                    "    out = out.split('{{{name}}}').join(params.{camel});\n",
                    name = name,
                    camel = camel_case(name),
                ));
            }
            body.push_str("    return out;\n");
            body.push_str("  }\n\n");
        }

        body.push_str(&format!(
            "  static encode(fields: {fields}): Uint8Array {{\n",
            fields = fields_name,
        ));
        body.push_str(&format!(
            "    return serializeMessage(fields, {class}.MODULE_KEY, {class}.MESSAGE_KEY, (w, f) => {{\n",
            class = class_name,
        ));
        for member in &members {
            let accessor = format!("f.{}", camel_case(&member.name));
            let stmt = emit_write_for_member(self, "w", &accessor, &member.ty, &path, "      ");
            body.push_str(&stmt);
        }
        body.push_str("    });\n");
        body.push_str("  }\n\n");

        body.push_str(&format!(
            "  static decode(bytes: Uint8Array): {{ header: MessageHeader; fields: {fields} }} {{\n",
            fields = fields_name,
        ));
        body.push_str(&format!(
            "    return deserializeMessage<{fields}>(bytes, (r) => ({{\n",
            fields = fields_name,
        ));
        for member in &members {
            let expr = emit_read_for_member(self, "r", &member.ty, &path);
            body.push_str(&format!("      {}: {},\n", camel_case(&member.name), expr,));
        }
        body.push_str("    }));\n");
        body.push_str("  }\n");

        body.push_str("}\n\n");

        Ok(Some(MessageEntry {
            class_name,
            fields_name,
            topic_const_name,
            topic_template: topic,
            module_key,
            message_key,
        }))
    }
}

fn collect_topic_params(template: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut chars = template.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '{' {
            let mut name = String::new();
            while let Some(&next) = chars.peek() {
                chars.next();
                if next == '}' {
                    break;
                }
                name.push(next);
            }
            if !name.is_empty() && !out.contains(&name) {
                out.push(name);
            }
        }
    }
    out
}
