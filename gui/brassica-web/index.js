/* To build (in ../brassica-interop-wasm/):
 * wasm32-wasi-cabal build brassica-interop-wasm
 * wizer --allow-wasi --wasm-bulk-memory true "$(wasm32-wasi-cabal list-bin -v0 brassica-interop-wasm)" -o "./dist/brassica-interop-wasm.wasm"
 */

import { WASI } from "@bjorn3/browser_wasi_shim";
import Split from "split.js";

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
const viewLive = document.getElementById("view-live");

function updateForm(reportRules, needsLive) {
    if (needsLive && !viewLive.checked) return;

    const data = new FormData(form);
    const rules = data.get("rules");
    const words = data.get("words");
    const highlightMode = data.get("highlightMode");

    const output = applyChanges(rules, words, reportRules, highlightMode);
    document.getElementById("results").innerHTML = output;
}

form.addEventListener("submit", (event) => {
    event.preventDefault();
    const reportRules = event.submitter.id === "report-btn";
    updateForm(reportRules, false);
});

const rulesArea = document.getElementById("rules");
rulesArea.addEventListener("input", (event) => updateForm(false, true));

const wordsArea = document.getElementById("words");
wordsArea.addEventListener("input", (event) => updateForm(false, true));

const exampleSelect = document.getElementById("examples");
const exampleMsg = "This will overwrite your current rules and lexicon. Are you sure you want to proceed?";
exampleSelect.addEventListener("change", async (event) => {
    const value = exampleSelect.value;
    if (value === "") return;

    if (!window.confirm(exampleMsg)) return;

    const bscFile = "examples/" + value + ".bsc";
    const lexFile = "examples/" + value + ".lex";

    const bsc = await fetch(bscFile).then((response) => response.text());
    const lex = await fetch(lexFile).then((response) => response.text());

    document.getElementById("rules").value = bsc;
    document.getElementById("words").value = lex;
});

var split = Split(["#rules-div", "#words-div", "#results-div"]);

const blurb = document.getElementById("blurb");
const blurbHeader = document.getElementById("blurb-header");
blurb.addEventListener("toggle", (event) => {
    if (blurb.open) {
        blurbHeader.innerHTML = "Click to close";
    } else {
        blurbHeader.innerHTML = "Click to open";
    }
});
