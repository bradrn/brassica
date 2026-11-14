import { WASI } from "@bjorn3/browser_wasi_shim";

let wasmResolve;
let wasmReady = new Promise((r) => { wasmResolve = r; });

const encoder = new TextEncoder();
const decoder = new TextDecoder();

// adapted from https://github.com/fourmolu/fourmolu/blob/main/web/worker/index.js
function withBytesPtr(hs, bytes, callback) {
    const len = bytes.byteLength;
    const ptr = hs.malloc(len);
    try {
        new Uint8Array(hs.memory.buffer, ptr, len).set(bytes);
        callback(ptr, len);
    } finally {
        hs.free(ptr);
    }
};

function decodeStableCStringLen(hs, stableCStringLen) {
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

// based on https://www.sitepen.com/blog/using-webassembly-with-web-workers
onmessage = function(e) {
    const data = e.data;
    if (data.type === "init") {
        const wasi = new WASI([], [], []);
        WebAssembly.instantiateStreaming(
            fetch("brassica-interop-wasm.wasm"),
            {"wasi_snapshot_preview1": wasi.wasiImport}
        ).then((wasm) => {
            wasi.inst = wasm.instance;
            const hs = wasm.instance.exports;
            wasmResolve(hs);
            postMessage({method: "_init"});
        });
    } else if (data.type === "dispatch") {
        wasmReady.then((hs) => {
            const req = encoder.encode(JSON.stringify(data.json));
            let resp;
            withBytesPtr(hs, req, (reqPtr, reqLen) => {
                const respHs = hs.dispatch_hs(reqPtr, reqLen);
                resp = JSON.parse(decodeStableCStringLen(hs, respHs));
            });
            postMessage(resp);
        });
    }
};
