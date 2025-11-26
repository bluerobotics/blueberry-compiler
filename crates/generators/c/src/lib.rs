use blueberry_ast::Definition;
use blueberry_codegen_core::{
    CodegenError, GeneratedFile, MESSAGE_HEADER_SIZE, MessageSpec, PrimitiveType, TopicFormat,
    TopicPlaceholder, collect_messages, snake_case_path, topic_format, uppercase_path,
};
use genco::lang::c::Tokens;
use genco::quote;

const OUTPUT_PATH: &str = "c/messages.h";
const RUNTIME_PATH: &str = "c/runtime.h";
const TOPIC_BUFFER_LEN: usize = 256;

pub fn generate(definitions: &[Definition]) -> Result<Vec<GeneratedFile>, CodegenError> {
    let messages = collect_messages(definitions)?;
    if messages.is_empty() {
        return Ok(Vec::new());
    }
    let generator = CGenerator::new(&messages);
    Ok(vec![
        GeneratedFile {
            path: RUNTIME_PATH.to_string(),
            contents: generator.render_runtime(),
        },
        GeneratedFile {
            path: OUTPUT_PATH.to_string(),
            contents: generator.render_messages(),
        },
    ])
}

struct CGenerator<'a> {
    messages: &'a [MessageSpec],
}

impl<'a> CGenerator<'a> {
    fn new(messages: &'a [MessageSpec]) -> Self {
        Self { messages }
    }

    fn render_runtime(&self) -> String {
        let helpers = helpers_tokens();
        let tokens: Tokens = quote! {
            #pragma once

            #include <stddef.h>
            #include <stdint.h>
            #include <string.h>

            #define BLUEBERRY_MESSAGE_HEADER_SIZE $(MESSAGE_HEADER_SIZE)u

            typedef struct {
                uint16_t payload_words;
                uint16_t flags;
                uint16_t module_key;
                uint16_t message_key;
            } blueberry_message_header_t;

            $helpers
        };
        tokens.to_file_string().expect("render runtime")
    }

    fn render_messages(&self) -> String {
        let message_blocks: Vec<Tokens> =
            self.messages.iter().map(|m| self.emit_message(m)).collect();

        let mut tokens: Tokens = quote! {
            #pragma once

            #include <stdbool.h>
            #include <stdio.h>
            #include <string.h>
            #include "runtime.h"

            #define BLUEBERRY_MAX_TOPIC_LENGTH $(TOPIC_BUFFER_LEN)u

            typedef int (*blueberry_publish_fn)(const char *topic, const uint8_t *frame, size_t frame_len, void *user_data);
        };

        for (idx, block) in message_blocks.iter().enumerate() {
            if idx > 0 {
                tokens.push();
                tokens.push();
            } else {
                tokens.push();
            }
            tokens.extend(block.clone());
        }

        tokens.to_file_string().expect("render messages")
    }

    fn emit_message(&self, message: &MessageSpec) -> Tokens {
        let snake = snake_case_path(&message.scope, &message.name);
        let struct_ident = format!("blueberry_{}_t", snake);
        let struct_macro = format!(
            "{}_STRUCT_SIZE",
            uppercase_path(&message.scope, &message.name)
        );
        let padded_macro = format!(
            "{}_PADDED_SIZE",
            uppercase_path(&message.scope, &message.name)
        );
        let payload_macro = format!(
            "{}_PAYLOAD_WORDS",
            uppercase_path(&message.scope, &message.name)
        );
        let module_macro = format!(
            "{}_MODULE_KEY",
            uppercase_path(&message.scope, &message.name)
        );
        let message_macro = format!(
            "{}_MESSAGE_KEY",
            uppercase_path(&message.scope, &message.name)
        );
        let topic_macro = format!(
            "{}_TOPIC_FMT",
            uppercase_path(&message.scope, &message.name)
        );
        let topic = topic_format(&message.topic);
        let topic_literal = blueberry_codegen_core::quoted_string(&topic.template);
        let struct_ident_q = struct_ident.clone();
        let struct_macro_q = struct_macro.clone();
        let padded_macro_q = padded_macro.clone();
        let payload_macro_q = payload_macro.clone();
        let module_macro_q = module_macro.clone();
        let message_macro_q = message_macro.clone();
        let topic_macro_q = topic_macro.clone();

        let pack = self.emit_pack(message, &struct_ident, &padded_macro);
        let parse = self.emit_parse(message, &struct_ident, &struct_macro);
        let from_frame =
            self.emit_from_frame(message, &struct_ident, &module_macro, &message_macro);
        let callback = self.emit_callback(message, &struct_ident);
        let publish = self.emit_publish(
            message,
            &struct_ident,
            &padded_macro,
            &payload_macro,
            &module_macro,
            &message_macro,
            &topic,
        );

        quote! {
            typedef struct {
                $(for field in &message.fields =>
                    $(c_field_type(field.primitive)) $(field.name.clone());
                )
            } $struct_ident_q;

            enum {
                $struct_macro_q = $(message.field_payload_size()),
                $padded_macro_q = $(message.padded_payload_size()),
                $payload_macro_q = $(message.payload_words()),
                $module_macro_q = $(message.module_key),
                $message_macro_q = $(message.message_key)
            };

            static const char $topic_macro_q[] = $topic_literal;

            $pack

            $parse

            $from_frame

            $callback

            $publish
        }
    }

    fn emit_pack(&self, message: &MessageSpec, struct_ident: &str, padded_macro: &str) -> Tokens {
        let pack_fn = format!("{}_pack", snake_case_path(&message.scope, &message.name));
        let writes: Tokens = quote! {
            $(for field in &message.fields =>
                $(c_writer(field.primitive))(msg->$(field.name.clone()), payload + offset);
                offset += $(field.primitive.size());
            )
        };

        quote! {
            static inline void $pack_fn(const $struct_ident *msg, uint8_t *payload) {
                memset(payload, 0, $padded_macro);
                size_t offset = 0;
                $writes
            }
        }
    }

    fn emit_parse(&self, message: &MessageSpec, struct_ident: &str, struct_macro: &str) -> Tokens {
        let parse_fn = format!("{}_parse", snake_case_path(&message.scope, &message.name));
        let reads: Tokens = quote! {
            $(for field in &message.fields =>
                out->$(field.name.clone()) = $(c_reader(field.primitive))(payload + offset);
                offset += $(field.primitive.size());
            )
        };

        quote! {
            static inline int $parse_fn(const uint8_t *payload, size_t payload_len, $struct_ident *out) {
                if (payload_len < $struct_macro) { return -1; }
                size_t offset = 0;
                $reads
                return 0;
            }
        }
    }

    fn emit_from_frame(
        &self,
        message: &MessageSpec,
        struct_ident: &str,
        module_macro: &str,
        message_macro: &str,
    ) -> Tokens {
        let snake_name = snake_case_path(&message.scope, &message.name);
        let from_frame_fn = format!("{}_from_frame", snake_name);
        let parse_fn = format!("{}_parse", snake_name);

        quote! {
            static inline int $from_frame_fn(const uint8_t *frame, size_t frame_len, $struct_ident *out) {
                blueberry_message_header_t header;
                if (blueberry_read_header(frame, frame_len, &header) != 0) { return -1; }
                if (header.module_key != $module_macro || header.message_key != $message_macro) { return -2; }
                size_t payload_len = (size_t)header.payload_words * 4u;
                if (frame_len < BLUEBERRY_MESSAGE_HEADER_SIZE + payload_len) { return -3; }
                return $parse_fn(frame + BLUEBERRY_MESSAGE_HEADER_SIZE, payload_len, out);
            }
        }
    }

    fn emit_callback(&self, message: &MessageSpec, struct_ident: &str) -> Tokens {
        let snake_name = snake_case_path(&message.scope, &message.name);
        let callback_ident = format!("{}_callback", snake_name);
        let callback_ident_q = callback_ident.clone();
        let dispatch_fn = format!("{}_dispatch", snake_name);
        let from_frame_fn = format!("{}_from_frame", snake_name);

        quote! {
            typedef void (*$callback_ident_q)(const $struct_ident *msg, void *user_data);

            static inline void $dispatch_fn(const uint8_t *frame, size_t frame_len, $callback_ident callback, void *user_data) {
                $struct_ident msg;
                if ($from_frame_fn(frame, frame_len, &msg) == 0) { callback(&msg, user_data); }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn emit_publish(
        &self,
        message: &MessageSpec,
        struct_ident: &str,
        padded_macro: &str,
        payload_macro: &str,
        module_macro: &str,
        message_macro: &str,
        topic: &TopicFormat,
    ) -> Tokens {
        let snake_name = snake_case_path(&message.scope, &message.name);
        let publish_fn = format!("{}_publish", snake_name);
        let pack_fn = format!("{}_pack", snake_name);
        let fmt_literal = format!("\"{}\"", topic.template);
        let fmt_args = c_format_arguments(&topic.placeholders);

        let args_branch: Tokens = if fmt_args.is_empty() {
            quote!(int topic_len = snprintf(topic, sizeof(topic), $fmt_literal);)
        } else {
            quote!(int topic_len = snprintf(topic, sizeof(topic), $fmt_literal, $(for arg in &fmt_args join (, ) => $arg););)
        };

        quote! {
            static inline int $publish_fn(blueberry_publish_fn publish, void *user_data, const char *device_type, const char *nid, const $struct_ident *msg) {
                uint8_t frame[BLUEBERRY_MESSAGE_HEADER_SIZE + $padded_macro] = {0};
                blueberry_message_header_t header = {
                    .payload_words = $payload_macro,
                    .flags = 0,
                    .module_key = $module_macro,
                    .message_key = $message_macro,
                };
                blueberry_write_header(&header, frame);
                $pack_fn(msg, frame + BLUEBERRY_MESSAGE_HEADER_SIZE);
                char topic[BLUEBERRY_MAX_TOPIC_LENGTH];
                $args_branch
                if (topic_len < 0 || (size_t)topic_len >= sizeof(topic)) { return -4; }
                return publish(topic, frame, sizeof(frame), user_data);
            }
        }
    }
}

fn c_writer(primitive: PrimitiveType) -> Tokens {
    match primitive {
        PrimitiveType::Bool | PrimitiveType::Char | PrimitiveType::Octet => {
            quote!(blueberry_write_u8)
        }
        PrimitiveType::I16 => quote!(blueberry_write_i16),
        PrimitiveType::U16 => quote!(blueberry_write_u16),
        PrimitiveType::I32 => quote!(blueberry_write_i32),
        PrimitiveType::U32 => quote!(blueberry_write_u32),
        PrimitiveType::I64 => quote!(blueberry_write_i64),
        PrimitiveType::U64 => quote!(blueberry_write_u64),
        PrimitiveType::F32 => quote!(blueberry_write_f32),
        PrimitiveType::F64 => quote!(blueberry_write_f64),
    }
}

fn c_reader(primitive: PrimitiveType) -> Tokens {
    match primitive {
        PrimitiveType::Bool | PrimitiveType::Char | PrimitiveType::Octet => {
            quote!(blueberry_read_u8)
        }
        PrimitiveType::I16 => quote!(blueberry_read_i16),
        PrimitiveType::U16 => quote!(blueberry_read_u16),
        PrimitiveType::I32 => quote!(blueberry_read_i32),
        PrimitiveType::U32 => quote!(blueberry_read_u32),
        PrimitiveType::I64 => quote!(blueberry_read_i64),
        PrimitiveType::U64 => quote!(blueberry_read_u64),
        PrimitiveType::F32 => quote!(blueberry_read_f32),
        PrimitiveType::F64 => quote!(blueberry_read_f64),
    }
}

fn c_field_type(primitive: PrimitiveType) -> Tokens {
    match primitive {
        PrimitiveType::Bool => quote!(uint8_t),
        PrimitiveType::Char => quote!(char),
        PrimitiveType::Octet => quote!(uint8_t),
        PrimitiveType::I16 => quote!(int16_t),
        PrimitiveType::U16 => quote!(uint16_t),
        PrimitiveType::I32 => quote!(int32_t),
        PrimitiveType::U32 => quote!(uint32_t),
        PrimitiveType::I64 => quote!(int64_t),
        PrimitiveType::U64 => quote!(uint64_t),
        PrimitiveType::F32 => quote!(float),
        PrimitiveType::F64 => quote!(double),
    }
}

fn c_format_arguments(placeholders: &[TopicPlaceholder]) -> Vec<Tokens> {
    placeholders
        .iter()
        .map(|placeholder| match placeholder {
            TopicPlaceholder::DeviceType => quote!(device_type),
            TopicPlaceholder::Nid => quote!(nid),
        })
        .collect()
}

fn helpers_tokens() -> Tokens {
    quote! {
        static inline void blueberry_write_u8(uint8_t value, uint8_t *buf) {
            buf[0] = value;
        }

        static inline uint8_t blueberry_read_u8(const uint8_t *buf) {
            return buf[0];
        }

        static inline void blueberry_write_i16(int16_t value, uint8_t *buf) {
            buf[0] = (uint8_t)(value & 0xFF);
            buf[1] = (uint8_t)((value >> 8) & 0xFF);
        }

        static inline int16_t blueberry_read_i16(const uint8_t *buf) {
            return (int16_t)((int16_t)buf[0] | ((int16_t)buf[1] << 8));
        }

        static inline void blueberry_write_u16(uint16_t value, uint8_t *buf) {
            buf[0] = (uint8_t)(value & 0xFFu);
            buf[1] = (uint8_t)((value >> 8) & 0xFFu);
        }

        static inline uint16_t blueberry_read_u16(const uint8_t *buf) {
            return (uint16_t)((uint16_t)buf[0] | ((uint16_t)buf[1] << 8));
        }

        static inline void blueberry_write_i32(int32_t value, uint8_t *buf) {
            buf[0] = (uint8_t)(value & 0xFF);
            buf[1] = (uint8_t)((value >> 8) & 0xFF);
            buf[2] = (uint8_t)((value >> 16) & 0xFF);
            buf[3] = (uint8_t)((value >> 24) & 0xFF);
        }

        static inline int32_t blueberry_read_i32(const uint8_t *buf) {
            return (int32_t)((int32_t)buf[0]
                | ((int32_t)buf[1] << 8)
                | ((int32_t)buf[2] << 16)
                | ((int32_t)buf[3] << 24));
        }

        static inline void blueberry_write_u32(uint32_t value, uint8_t *buf) {
            buf[0] = (uint8_t)(value & 0xFFu);
            buf[1] = (uint8_t)((value >> 8) & 0xFFu);
            buf[2] = (uint8_t)((value >> 16) & 0xFFu);
            buf[3] = (uint8_t)((value >> 24) & 0xFFu);
        }

        static inline uint32_t blueberry_read_u32(const uint8_t *buf) {
            return (uint32_t)((uint32_t)buf[0]
                | ((uint32_t)buf[1] << 8)
                | ((uint32_t)buf[2] << 16)
                | ((uint32_t)buf[3] << 24));
        }

        static inline void blueberry_write_i64(int64_t value, uint8_t *buf) {
            for (int i = 0; i < 8; ++i) {
                buf[i] = (uint8_t)((uint64_t)value >> (8 * i));
            }
        }

        static inline int64_t blueberry_read_i64(const uint8_t *buf) {
            int64_t value = 0;
            for (int i = 0; i < 8; ++i) {
                value |= ((int64_t)buf[i]) << (8 * i);
            }
            return value;
        }

        static inline void blueberry_write_u64(uint64_t value, uint8_t *buf) {
            for (int i = 0; i < 8; ++i) {
                buf[i] = (uint8_t)(value >> (8 * i));
            }
        }

        static inline uint64_t blueberry_read_u64(const uint8_t *buf) {
            uint64_t value = 0;
            for (int i = 0; i < 8; ++i) {
                value |= ((uint64_t)buf[i]) << (8 * i);
            }
            return value;
        }

        static inline void blueberry_write_f32(float value, uint8_t *buf) {
            uint32_t as_int;
            memcpy(&as_int, &value, sizeof(float));
            blueberry_write_u32(as_int, buf);
        }

        static inline float blueberry_read_f32(const uint8_t *buf) {
            uint32_t as_int = blueberry_read_u32(buf);
            float value;
            memcpy(&value, &as_int, sizeof(float));
            return value;
        }

        static inline void blueberry_write_f64(double value, uint8_t *buf) {
            uint64_t as_int;
            memcpy(&as_int, &value, sizeof(double));
            blueberry_write_u64(as_int, buf);
        }

        static inline double blueberry_read_f64(const uint8_t *buf) {
            uint64_t as_int = blueberry_read_u64(buf);
            double value;
            memcpy(&value, &as_int, sizeof(value));
            return value;
        }

        static inline void blueberry_write_header(const blueberry_message_header_t *header, uint8_t *frame) {
            blueberry_write_u16(header->payload_words, frame + 0);
            blueberry_write_u16(header->flags, frame + 2);
            blueberry_write_u16(header->module_key, frame + 4);
            blueberry_write_u16(header->message_key, frame + 6);
        }

        static inline int blueberry_read_header(const uint8_t *frame, size_t frame_len, blueberry_message_header_t *out) {
            if (frame_len < BLUEBERRY_MESSAGE_HEADER_SIZE) {
                return -1;
            }
            out->payload_words = blueberry_read_u16(frame + 0);
            out->flags = blueberry_read_u16(frame + 2);
            out->module_key = blueberry_read_u16(frame + 4);
            out->message_key = blueberry_read_u16(frame + 6);
            return 0;
        }
    }
}
