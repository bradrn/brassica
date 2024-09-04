import Split from "split.js";

import {EditorState, EditorSelection} from "@codemirror/state"
import {EditorView, keymap, highlightSpecialChars, drawSelection, dropCursor,
        rectangularSelection, crosshairCursor, lineNumbers} from "@codemirror/view"
import {StreamLanguage, LanguageSupport, HighlightStyle, syntaxHighlighting,
        bracketMatching} from "@codemirror/language"
import {tags} from "@lezer/highlight"
import {defaultKeymap, history, historyKeymap} from "@codemirror/commands"
import {searchKeymap, highlightSelectionMatches} from "@codemirror/search"
import {closeBrackets, closeBracketsKeymap} from "@codemirror/autocomplete"

import {hs, withBytesPtr, decodeStableCStringLen, encoder, decoder} from "./interop.js";


/***********************
 * Haskell interop     *
 ***********************/

const results = hs.initResults();  // NB: not const on Haskell side!

function applyChanges(changes, words, sep, reportRules, inputMode, highlightMode, outputMode) {
    const inputChanges = encoder.encode(changes);
    const inputWords = encoder.encode(words);
    const sepEncoded = encoder.encode(sep);

    const reportRulesC = reportRules ? 1 : 0;

    var inModeC = 0;
    switch (inputMode) {
    case 'mdfStandard':  inModeC = 1; break;
    case 'mdfAlternate': inModeC = 1; break;
    }

    var hlModeC = 0;
    switch (highlightMode) {
    case 'differentToLastRun': hlModeC = 1; break;
    case 'differentToInput':   hlModeC = 2; break;
    }

    var outModeC = 0;
    switch (outputMode) {
    case 'mdf':     outModeC = 1; break;
    case 'mdfetym': outModeC = 2; break;
    case 'inout':   outModeC = 3; break;
    }

    var output = "";
    withBytesPtr(inputChanges, (inputChangesPtr, inputChangesLen) => {
        withBytesPtr(inputWords, (inputWordsPtr, inputWordsLen) => {
            withBytesPtr(sepEncoded, (sepPtr, sepLen) => {
                try {
                    const outputStableCStringLen = hs.parseTokeniseAndApplyRules_hs(
                        inputChangesPtr, inputChangesLen,
                        inputWordsPtr, inputWordsLen,
                        sepPtr, sepLen,
                        reportRules, inModeC, hlModeC, outModeC, results);
                    output = decodeStableCStringLen(outputStableCStringLen);
                } catch (err) {
                    output = err;
                }
            });
        });
    });
    return output;
}


/***********************
 * Syntax highlighting *
 ***********************/

const brassicaMainRules = [
    { token: "keyword",
      regex: />|#|\(|\)|{|}|\\|\^|%|~|\*|nohighlight|extra|filter|report|@[0-9]+|@\?/
    },
    { token: "separator",
      regex: /\/|_|→|->/
    },
    { token: "controlOperator",  // actually features - but why not?
      regex: /\$[^\s#[\](){}>\\→/_^%~*@$]+(#[^\s#[\](){}>\\→/_^%~*@$]+)?/
    },
    { token: "meta",
      regex: /^-(x|1|ltr|rtl|\?\?|\?)/
    },
    { token: "variableName",  // categories - there's no closer tag type
      regex: /\[.*?\]/
    }
];

const brassicaLang = StreamLanguage.define({
    name: "Brassica",
    startState: (i) => { return { categories: [], cblock: false }},
    token: function (stream, state) {
        if (stream.match(/;.*$/)) {
            return "comment";
        }
        if (state.cblock) {
            if (stream.match(/^end$/)) {
                state.cblock = false;
                return "keyword";
            }
            if (stream.match(/feature|auto/)) {
                return "keyword";
            }
            const catMatch = stream.match(/^(\S+)(?=\s+=)/);
            if (catMatch) {
                state.categories.push(catMatch[1]);
                return "variableName";
            }
        } else {
            if (stream.match(/^new categories( nohighlight)?$/)) {
                state.categories = []
                state.cblock = true;
                return "keyword";
            }
            if (stream.match(/^categories$/)) {
                state.cblock = true;
                return "keyword";
            }
            for (let rule of brassicaMainRules) {
                if (stream.match(rule.regex)) {
                    return rule.token;
                }
            }
            for (let category of state.categories) {
                if (stream.match(category)) {
                    return "variableName";
                }
            }
        }
        stream.next();
        return null;
    }
});

const brassicaHighlightStyle = HighlightStyle.define([
    {tag: tags.keyword, color: "#00f"},
    {tag: tags.separator, fontWeight: "bold"},
    {tag: tags.controlOperator,  color: "rgb(34,139,34)"},
    {tag: tags.meta, color: "rgb(0,128,128)"},
    {tag: tags.variableName, backgroundColor: "rgb(245,245,220)"},
    {tag: tags.comment, "color": "rgb(0,128,0)"}
]);


/***********************
 * Set up content      *
 ***********************/

Split(["#rules-div", "#words-div", "#results-div"]);

const form = document.getElementById("brassica-form");
const viewLive = document.getElementById("view-live");
const wordsArea = document.getElementById("words");
const resultsDiv = document.getElementById("results");

const urlParams = new URLSearchParams(window.location.search);
wordsArea.value = urlParams.get("w");

let rulesEditor = new EditorView({
    doc: urlParams.get("r"),
    extensions: [
        highlightSpecialChars(),
        history(),
        drawSelection(),
        dropCursor(),
        EditorState.allowMultipleSelections.of(true),
        syntaxHighlighting(brassicaHighlightStyle),
        bracketMatching(),
        closeBrackets(),
        rectangularSelection(),
        crosshairCursor(),
        highlightSelectionMatches(),
        new LanguageSupport(brassicaLang),
        keymap.of([
            ...closeBracketsKeymap,
            ...defaultKeymap,
            ...searchKeymap,
            ...historyKeymap
        ])
    ],
    parent: document.getElementById("rules"),
})

const hlNoneRadio = document.getElementById("hl-none");
const hlLastRadio = document.getElementById("hl-last");
const hlInputRadio = document.getElementById("hl-input");

const inWordlistRadio = document.getElementById("in-wordlist");
const inMdfStandardRadio = document.getElementById("in-mdfstandard");
const inMdfAlternateRadio = document.getElementById("in-mdfalternate");

const fmtWordlistRadio = document.getElementById("fmt-wordlist");
const fmtInoutRadio = document.getElementById("fmt-inout");
const fmtMdfRadio = document.getElementById("fmt-mdf");
const fmtMdfEtymRadio = document.getElementById("fmt-mdfetym");

function updateForm(reportRules, needsLive) {
    if (needsLive && !viewLive.checked) return;

    const data = new FormData(form);
    const rules = rulesEditor.state.doc.toString();
    const words = data.get("words");
    const sep = data.get("sep");
    const highlightMode = data.get("highlightMode");
    const inputFormat = data.get("inputFormat");
    const outputFormat = data.get("outputFormat");

    const output = applyChanges(rules, words, sep, reportRules, inputFormat, highlightMode, outputFormat);
    resultsDiv.innerHTML = "<pre>" + output + "</pre>";
}

form.addEventListener("submit", (event) => {
    event.preventDefault();
    const reportRules = event.submitter.id === "report-btn";
    updateForm(reportRules, false);
});

// live highlight
wordsArea          .addEventListener("input", (event) => updateForm(false, true));
hlNoneRadio        .addEventListener("input", (event) => updateForm(false, true));
hlLastRadio        .addEventListener("input", (event) => updateForm(false, true));
hlInputRadio       .addEventListener("input", (event) => updateForm(false, true));
inWordlistRadio    .addEventListener("input", (event) => updateForm(false, true));
inMdfStandardRadio .addEventListener("input", (event) => updateForm(false, true));
inMdfAlternateRadio.addEventListener("input", (event) => updateForm(false, true));
fmtWordlistRadio   .addEventListener("input", (event) => updateForm(false, true));
fmtInoutRadio      .addEventListener("input", (event) => updateForm(false, true));
fmtMdfRadio        .addEventListener("input", (event) => updateForm(false, true));
fmtMdfEtymRadio    .addEventListener("input", (event) => updateForm(false, true));

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

    let spec = {from: 0, to: rulesEditor.state.doc.length, insert: bsc};
    rulesEditor.dispatch(rulesEditor.state.update({changes: spec}))
    wordsArea.value = lex;
});

const blurb = document.getElementById("blurb");
const blurbHeader = document.getElementById("blurb-header");
blurb.addEventListener("toggle", (event) => {
    if (blurb.open) {
        blurbHeader.innerHTML = "Click to close";
    } else {
        blurbHeader.innerHTML = "Click to open";
    }
});

// adapted from https://stackoverflow.com/a/33542499/
function save(filename, data) {
    const blob = new Blob([data]);
    const elem = window.document.createElement('a');
    const url = window.URL.createObjectURL(blob);
    elem.href = url;
    elem.download = filename;
    elem.style.display = 'none';
    document.body.appendChild(elem);
    elem.click();
    document.body.removeChild(elem);
    URL.revokeObjectURL(url);
}

document.getElementById("download-rules").addEventListener("click", (event) => {
    event.preventDefault();
    save("rules.bsc", rulesEditor.state.doc.toString());
});
document.getElementById("download-words").addEventListener("click", (event) => {
    event.preventDefault();
    save("words.lex", wordsArea.value);
});

document.getElementById("select-all-rules").addEventListener("click", (event) => {
    let sel = EditorSelection.range(0, rulesEditor.state.doc.length);
    rulesEditor.dispatch(rulesEditor.state.update({selection: sel}));
});
document.getElementById("select-all-words").addEventListener("click", (event) => {
    wordsArea.select();
});
document.getElementById("select-all-results").addEventListener("click", (event) => {
    // see https://stackoverflow.com/a/1173319
    var range = document.createRange();
    range.selectNode(resultsDiv);
    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);
});

const inputFileRules = document.getElementById("input-file-rules");
inputFileRules.addEventListener("change", (event) => {
    const file = inputFileRules.files[0];
    if (file) {
        const reader = new FileReader();
        reader.onload = (e) => {
            let spec = {from: 0, to: rulesEditor.state.doc.length, insert: e.target.result};
            rulesEditor.dispatch(rulesEditor.state.update({changes: spec}))
        };
        reader.readAsText(file);
    }
});
document
    .getElementById("open-rules")
    .addEventListener("click", (event) => inputFileRules.click());

const inputFileWords = document.getElementById("input-file-words");
inputFileWords.addEventListener("change", (event) => {
    const file = inputFileWords.files[0];
    if (file) {
        const reader = new FileReader();
        reader.onload = (e) => {
            wordsArea.value = e.target.result;
        };
        reader.readAsText(file);
    }
});
document
    .getElementById("open-words")
    .addEventListener("click", (event) => inputFileWords.click());

function reselectRadios(event) {
    if (inMdfStandardRadio.checked || inMdfAlternateRadio.checked) {
        fmtMdfRadio.disabled = false;
        fmtMdfEtymRadio.disabled = false;
    } else {
        if (fmtMdfRadio.checked || fmtMdfEtymRadio.checked) {
            fmtWordlistRadio.checked = true;
        }
        fmtMdfRadio.disabled = true;
        fmtMdfEtymRadio.disabled = true;
    }
}

inWordlistRadio    .addEventListener("input", reselectRadios)
inMdfStandardRadio .addEventListener("input", reselectRadios)
inMdfAlternateRadio.addEventListener("input", reselectRadios)

const synchroniseScroll = document.getElementById("synchronise-scroll");
var blockScrollTrackingEvent = false;

wordsArea.addEventListener("scroll", function (event) {
    if (!synchroniseScroll.checked) return;

    if (blockScrollTrackingEvent) {
        blockScrollTrackingEvent = false;
    } else {
        const ratio = wordsArea.scrollTop / wordsArea.scrollHeight;
        blockScrollTrackingEvent = true;
        resultsDiv.scrollTop = ratio * resultsDiv.scrollHeight;
    }
});

resultsDiv.addEventListener("scroll", function (event) {
    if (!synchroniseScroll.checked) return;

    if (blockScrollTrackingEvent) {
        blockScrollTrackingEvent = false;
    } else {
        const ratio = resultsDiv.scrollTop / resultsDiv.scrollHeight;
        blockScrollTrackingEvent = true;
        wordsArea.scrollTop = ratio * wordsArea.scrollHeight;
    }
});
