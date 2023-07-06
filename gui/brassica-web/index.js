/* To build (in ../brassica-interop-wasm/):
 * wasm32-wasi-cabal build brassica-interop-wasm --project-file=cabal-wasm.project
 * wizer --allow-wasi --wasm-bulk-memory true "$(wasm32-wasi-cabal list-bin -v0 brassica-interop-wasm --project-file cabal-wasm.project)" -o "./dist/brassica-interop-wasm.wasm"
 */

import { WASI } from "@bjorn3/browser_wasi_shim";

const wasi = new WASI([], [], []);
const wasm = await WebAssembly.instantiateStreaming(
    fetch("brassica-interop-wasm.wasm"),
    {"wasi_snapshot_preview1": wasi.wasiImport}
);
wasi.inst = wasm.instance;
const hs = wasm.instance.exports;

// adapted from https://github.com/fourmolu/fourmolu/blob/main/web/worker/index.js
function withBytesPtr(bytes, callback) {
    const len = bytes.byteLength;
    const ptr = hs.malloc(len);
    try {
        new Uint8Array(hs.memory.buffer, ptr, len).set(bytes);
        callback(ptr, len);
    } finally {
        hs.free(ptr);
    }
};

function decodeStableCStringLen(stableCStringLen) {
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

const encoder = new TextEncoder();
const decoder = new TextDecoder();

const results = hs.initResults();  // NB: not const on Haskell side!

function applyChanges(changes, words, reportRules, highlightMode) {
    const inputChanges = encoder.encode(changes);
    const inputWords = encoder.encode(words);
    const reportRulesC = reportRules ? 1 : 0;
    var hlModeC = 0;
    switch (highlightMode) {
    case 'differentToLastRun': hlModeC = 1; break;
    case 'differentToInput':   hlModeC = 2; break;
    }
    var output = "";
    withBytesPtr(inputChanges, (inputChangesPtr, inputChangesLen) => {
        withBytesPtr(inputWords, (inputWordsPtr, inputWordsLen) => {
            try {
                const outputStableCStringLen = hs.parseTokeniseAndApplyRules_hs(
                    inputChangesPtr, inputChangesLen,
                    inputWordsPtr, inputWordsLen,
                    reportRules, 0, hlModeC, 1, results);
                output = decodeStableCStringLen(outputStableCStringLen);
            } catch (err) {
                output = err;
            }
        });
    });
    return output;
}

const form = document.getElementById("brassica-form");
form.addEventListener("submit", (event) => {
    event.preventDefault();
    const data = new FormData(form);
    const rules = data.get("rules");
    const words = data.get("words");
    const highlightMode = data.get("highlightMode");
    const reportRules = event.submitter.id === "report-btn";
    const output = applyChanges(rules, words, reportRules, highlightMode);
    document.getElementById("results").innerHTML = output;
});
