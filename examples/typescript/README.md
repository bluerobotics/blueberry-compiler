# Blueberry TypeScript example

Mirrors `examples/python/main.py`. Demonstrates using the generated TypeScript
bindings (emitted by `blueberry-cli --emit-typescript`) together with the
`blueberry-serde-ts` runtime to exchange messages with a Blueberry device over
UDP on port `0x4242`.

## Generate bindings

```bash
cargo run -p blueberry-cli -- \
    crates/parser/tests/fixtures/blueberry_full.idl \
    --emit-typescript \
    --output-dir examples/typescript
```

This drops `typescript/blueberry_messages.ts` and `typescript/index.ts` into
`examples/typescript/typescript/`.

## Install + typecheck

```bash
cd examples/typescript
npm ci
npx tsc --noEmit
```

`tsc --noEmit` runs typechecking only; it is also what the repo's CI uses to
verify that the generated TypeScript compiles cleanly against the runtime.

## Run

```bash
npm run main      # UDP exchange with a connected device
npm run use-full  # exhaustive primitive round-trip (no network)
```

The runtime lives at <https://github.com/eldinmiller/blueberry-serde-ts>.
