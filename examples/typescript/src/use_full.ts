/**
 * Exhaustive encode/decode round-trip across every primitive supported by
 * `BlueberryWriter` / `BlueberryReader`. Useful as a runtime smoke test.
 */

import {
  BlueberryReader,
  BlueberryWriter,
  serialize,
  deserialize,
} from 'blueberry-serde-ts';

interface AllPrimitives {
  flag: boolean;
  byte: number;
  u16: number;
  i16: number;
  u32: number;
  i32: number;
  u64: bigint;
  i64: bigint;
  f32: number;
  f64: number;
  greeting: string;
}

function encodeAllPrimitives(w: BlueberryWriter, f: AllPrimitives): void {
  w.writeU64(f.u64);
  w.writeI64(f.i64);
  w.writeF64(f.f64);
  w.writeU32(f.u32);
  w.writeI32(f.i32);
  w.writeF32(f.f32);
  w.writeU16(f.u16);
  w.writeI16(f.i16);
  w.writeU8(f.byte);
  w.writeBool(f.flag);
  w.writeString(f.greeting);
}

function decodeAllPrimitives(r: BlueberryReader): AllPrimitives {
  return {
    u64: r.readU64(),
    i64: r.readI64(),
    f64: r.readF64(),
    u32: r.readU32(),
    i32: r.readI32(),
    f32: r.readF32(),
    u16: r.readU16(),
    i16: r.readI16(),
    byte: r.readU8(),
    flag: r.readBool(),
    greeting: r.readString(),
  };
}

function main(): void {
  const original: AllPrimitives = {
    flag: true,
    byte: 0xab,
    u16: 0xbeef,
    i16: -1234,
    u32: 0xdeadbeef,
    i32: -42,
    u64: 0x1122334455667788n,
    i64: -1n,
    f32: 1.5,
    f64: Math.PI,
    greeting: 'hello, blueberry',
  };

  const bytes = serialize(original, encodeAllPrimitives);
  const decoded = deserialize<AllPrimitives>(bytes, decodeAllPrimitives);

  console.log('encoded   :', Buffer.from(bytes).toString('hex'));
  console.log('decoded   :', decoded);

  for (const key of Object.keys(original) as Array<keyof AllPrimitives>) {
    if (decoded[key] !== original[key]) {
      throw new Error(`round-trip mismatch for ${String(key)}`);
    }
  }
  console.log('round-trip ok');
}

main();
