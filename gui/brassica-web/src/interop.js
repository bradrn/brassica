import { WASI } from "@bjorn3/browser_wasi_shim";

const wasi = new WASI([], [], []);
const wasm = await WebAssembly.instantiateStreaming(
    fetch("brassica-interop-wasm.wasm"),
    {"wasi_snapshot_preview1": wasi.wasiImport}
);
wasi.inst = wasm.instance;
export const hs = wasm.instance.exports;

// adapted from https://github.com/fourmolu/fourmolu/blob/main/web/worker/index.js
export function withBytesPtr(bytes, callback) {
    const len = bytes.byteLength;
    const ptr = hs.malloc(len);
    try {
        new Uint8Array(hs.memory.buffer, ptr, len).set(bytes);
        callback(ptr, len);
    } finally {
        hs.free(ptr);
    }
};

export function decodeStableCStringLen(stableCStringLen) {
    try {
        const cstringptr = hs.getString(stableCStringLen);
        const cstringlen = hs.getStringLen(stableCStringLen);
        const outputBytes = new Uint8Array(hs.memory.buffer, cstringptr, cstringlen);
        var output = decoder.decode(outputBytes);
    } finally {
        hs.freeStableCStringLen(stableCStringLen);
    }
    return output;
};

export const encoder = new TextEncoder();
export const decoder = new TextDecoder();
