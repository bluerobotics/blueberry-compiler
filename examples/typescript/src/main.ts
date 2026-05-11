/**
 * Blueberry protocol test — communicate with a device over UDP.
 *
 * Mirrors examples/python/main.py.
 */

import dgram from 'node:dgram';

import { BLUEBERRY_PORT, emptyMessage, serializePacket } from 'blueberry-serde-ts';

import { decodeMessage } from '../typescript/index.js';

const DEVICE_HOST = '192.168.31.28';
const RECV_TIMEOUT_MS = 1000;

interface MessageClass {
  readonly MODULE_KEY: number;
  readonly MESSAGE_KEY: number;
  readonly name: string;
}

function requestPacketFor(cls: MessageClass): Uint8Array {
  return serializePacket([emptyMessage(cls.MODULE_KEY, cls.MESSAGE_KEY)]);
}

async function main(): Promise<void> {
  const messagesToRequest: MessageClass[] = [];
  const sock = dgram.createSocket('udp4');
  await new Promise<void>((resolve, reject) => {
    sock.once('error', reject);
    sock.bind(BLUEBERRY_PORT, '0.0.0.0', () => resolve());
  });

  console.info(`Communicating with ${DEVICE_HOST}:${BLUEBERRY_PORT} via UDP`);

  for (const cls of messagesToRequest) {
    const pkt = requestPacketFor(cls);
    await new Promise<void>((resolve, reject) => {
      sock.send(pkt, BLUEBERRY_PORT, DEVICE_HOST, (error) => {
        if (error) reject(error);
        else resolve();
      });
    });
    console.info(`Sent ${cls.name} request (${pkt.length} bytes)`);
  }

  console.info(`Waiting for responses (${RECV_TIMEOUT_MS}ms timeout)...`);
  const buf: number[] = [];
  const timeout = setTimeout(() => {
    sock.close();
  }, RECV_TIMEOUT_MS);

  sock.on('message', (data) => {
    buf.push(...data);
    const bytes = new Uint8Array(buf);
    const decoded = decodeMessage(bytes);
    console.info('  decoded:', decoded.kind);
  });

  sock.on('close', () => {
    clearTimeout(timeout);
    console.info('Receive finished (timeout)');
  });
}

void main();
