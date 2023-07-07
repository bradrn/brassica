import ace from "ace-builds";
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

ace.define('ace/mode/brassica', function(require, exports, module) {
    var oop = require("ace/lib/oop");
    var TextMode = require("ace/mode/text").Mode;
    var BrassicaHighlightRules = require("ace/mode/brassica_highlight_rules").BrassicaHighlightRules;

    var Mode = function() {
        this.HighlightRules = BrassicaHighlightRules;
    };
    oop.inherits(Mode, TextMode);

    (function() {
        // Extra logic goes here. (see below)
    }).call(Mode.prototype);

    exports.Mode = Mode;
});

ace.define('ace/mode/brassica_highlight_rules', function(require, exports, module) {
    var oop = require("ace/lib/oop");
    var TextHighlightRules = require("ace/mode/text_highlight_rules").TextHighlightRules;

    var BrassicaHighlightRules = function() {
        this.$rules = {
            start: [
                { token: "keyword",
                  regex: ">|#|\\(|\\)|{|}|\\\\|\\^|%|~|\\*|categories|end|new|feature|@[0-9]+|@\\?"
                },
                { token: "separator",
                  regex: "/|_|→"
                },
                { token: "comment",
                  regex: ";.*$"
                },
                { token: "category",
                  regex: "\\[.*?\\]"
                },
            ]
        };
        this.normalizeRules();
    };

    oop.inherits(BrassicaHighlightRules, TextHighlightRules);
    exports.BrassicaHighlightRules = BrassicaHighlightRules;
});

var editor = ace.edit("rules");
editor.session.setMode("ace/mode/brassica");
editor.renderer.setShowGutter(false);
editor.setHighlightActiveLine(false);
editor.renderer.setOptions({
    fontFamily: "monospace",
    fontSize: "inherit",
});

var markers = [];

function rehighlight() {
    // preliminary: remove all markers
    markers.forEach((id) => editor.session.removeMarker(id));

    const rules = editor.getValue();
    const rulesLines = rules.split("\n");

    // first pass: accumulate categories
    let categories = [];
    const featureRegex = /=|\/|feature/g;
    let inCategories = false;
    rulesLines.forEach((line) => {
        if (line.includes("categories")) {
            inCategories = true;
        } else if (line === "end") {
            inCategories = false;
        } else if (line.includes("feature")) {
            let lineParts = line.split(featureRegex);
            // every second part from the end is a category name,
            for (let i = lineParts.length-2; i>=0; i-=2) {
                categories.push(lineParts[i].trim());
            }
        } else if (inCategories) {
            let lineParts = line.split("=");
            if (lineParts.length > 1) {
                categories.push(lineParts[0].trim());
            }
        }
    });

    let catsRegex = new RegExp(categories.join("|"), "gd");

    // second pass: find row/column coords of matches
    // and add them as markers
    const Range = ace.require("ace/range").Range;
    let ranges = [];
    for (let row=0; row<rulesLines.length; ++row) {
        let results = rulesLines[row].matchAll(catsRegex);
        for (const result of results) {
            let r = new Range(
                row, result.index,
                row, result.index + result[0].length);
            let id = editor.session.addMarker(r, "regex_category", "text");
            markers.push(id);
        };
    }
}

const form = document.getElementById("brassica-form");
const viewLive = document.getElementById("view-live");

function updateForm(reportRules, needsLive) {
    if (needsLive && !viewLive.checked) return;

    const data = new FormData(form);
    const rules = editor.getValue();
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

editor.addEventListener("input", (event) => {
    rehighlight();
    updateForm(false, true);
});

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

    editor.setValue(bsc);
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
