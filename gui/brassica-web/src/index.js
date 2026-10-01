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
import {linter, Diagnostic} from "@codemirror/lint"


/***********************
 * Haskell interop     *
 ***********************/

const worker = new Worker(new URL("interop.js", import.meta.url));

var prevResults = null;

document.getElementById("results").innerHTML = "<i>Initialising...</i>"

worker.onmessage = (e) => {
    const data = e.data;
    if (data.method === "_init") {
        document.getElementById("results").innerHTML = "";
    } else if (data.method === "Error") {
        errorResult(data.message, data.highlights);
    } else if (data.method === "Rules") {
        prevResults = data.prev;
        rulesResult(data.output);
    } else if (data.method === "NotApplied") {
        highlightResult(data.highlights);
    } else {
        console.error(data);
    }
    endApply();
};


worker.postMessage({type: "init"});

function applyChanges(changes, words, sep, reportRules, inputMode, highlightMode, outputMode, timeout) {
    const req = {
        method: "Rules",
        changes: changes,
        input: words,
        report: reportRules,
        inFmt: inputMode,
        hlMode: highlightMode,
        outMode: outputMode,
        prev: prevResults,
        sep: sep,
        reqTimeout: timeout
    };
    beginApply();
    worker.postMessage({type: "dispatch", json: req});
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
      regex: /^-(x|h|1|ltr|rtl|\?\?|\?)/
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

var errors = [];
var highlights = [];

const brassicaLinter = linter(view => {
    let diagnostics = [];
    let doc = view.state.doc;
    for (let i = 0; i < errors.length; ++i) {
        diagnostics.push({
            from: doc.line(errors[i]).from,
            to: doc.line(errors[i]).to,
            severity: "error",
            message: "Syntax error"
        })
    }
    for (let i = 0; i < highlights.length; ++i) {
        diagnostics.push({
            from: doc.line(highlights[i]).from,
            to: doc.line(highlights[i]).to,
            severity: "info",
            message: "Unused rule"
        })
    }
    return diagnostics;
});


/***********************
 * Set up content      *
 ***********************/

Split(["#rules-div", "#words-div", "#results-div"]);

const form = document.getElementById("brassica-form");
const viewLive = document.getElementById("view-live");
const highlightUnused = document.getElementById("highlight-unused");
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
        brassicaLinter,
        keymap.of([
            ...closeBracketsKeymap,
            ...defaultKeymap,
            ...searchKeymap,
            ...historyKeymap
        ])
    ],
    dispatchTransactions: function (trs, view) {
        view.update(trs);
        if (trs.some((t) => t.docChanged)) {
            updateForm(null, true);
        }
    },
    parent: document.getElementById("rules"),
})

function forceLinterUpdate() {
    // see https://discuss.codemirror.net/t/3570/16
    let plugin = rulesEditor.plugin(brassicaLinter[1]);
    plugin.set = true;
    plugin.force();
}

const hlNoneRadio = document.getElementById("hl-none");
const hlLastRadio = document.getElementById("hl-last");
const hlInputRadio = document.getElementById("hl-input");
const hlSpecificRadio = document.getElementById("hl-specific");

const inWordlistRadio = document.getElementById("in-wordlist");
const inMdfStandardRadio = document.getElementById("in-mdfstandard");
const inMdfAlternateRadio = document.getElementById("in-mdfalternate");

const fmtWordlistRadio = document.getElementById("fmt-wordlist");
const fmtInoutRadio = document.getElementById("fmt-inout");
const fmtInoutPreserveRadio = document.getElementById("fmt-inoutpreserve");
const fmtMdfRadio = document.getElementById("fmt-mdf");
const fmtMdfEtymRadio = document.getElementById("fmt-mdfetym");

var wasLive = false;
var inprogress = false;

const timeout = 10000000;  // microseconds (10 s)

function beginApply() {
    inprogress = true;
    document.getElementById("apply-btn").disabled = true;
    document.getElementById("report-btn").disabled = true;
    document.getElementById("progress-bar").removeAttribute("value");
}

function endApply() {
    inprogress = false;
    document.getElementById("apply-btn").disabled = false;
    document.getElementById("report-btn").disabled = false;
    document.getElementById("progress-bar").value = 1;
}

function updateForm(reportRules, needsLive) {
    if (needsLive && !viewLive.checked && !inprogress)
        return;

    wasLive = needsLive;

    const data = new FormData(form);
    const rules = rulesEditor.state.doc.toString();
    const words = data.get("words");
    const sep = data.get("sep");
    const highlightMode = data.get("highlightMode");
    const inputFormat = data.get("inputFormat");
    const outputFormat = data.get("outputFormat");

    applyChanges(rules, words, sep, reportRules, inputFormat, highlightMode, outputFormat, timeout);
}

function rulesResult(output) {
    resultsDiv.classList.remove("disabled-error");
    resultsDiv.innerHTML = "<pre>" + output + "</pre>";

    errors = [];

    if (highlightUnused.checked) {
        const data = new FormData(form);
        const rules = rulesEditor.state.doc.toString();
        const words = data.get("words");
        const sep = data.get("sep");
        const inputFormat = data.get("inputFormat");
        applyChanges(rules, words, sep, 'ReportNotApplied', inputFormat, 'NoHighlight', 'WordsOnlyOutput', timeout);
    }
}

function errorResult(message, newErrors) {
    if (wasLive) {
        resultsDiv.classList.add("disabled-error");
    } else {
        resultsDiv.classList.remove("disabled-error");
        resultsDiv.innerHTML = "<pre>" + message + "</pre>";
    }

    errors = newErrors;
    forceLinterUpdate();
}

form.addEventListener("submit", (event) => {
    event.preventDefault();
    const reportRules =
          (event.submitter.id == "report-btn") ? "ReportApplied" : null;
    updateForm(reportRules, false);
});

// live highlight
wordsArea          .addEventListener("input", (event) => updateForm(null, true));
hlNoneRadio        .addEventListener("input", (event) => updateForm(null, true));
hlLastRadio        .addEventListener("input", (event) => updateForm(null, true));
hlInputRadio       .addEventListener("input", (event) => updateForm(null, true));
hlSpecificRadio    .addEventListener("input", (event) => updateForm(null, true));
inWordlistRadio    .addEventListener("input", (event) => updateForm(null, true));
inMdfStandardRadio .addEventListener("input", (event) => updateForm(null, true));
inMdfAlternateRadio.addEventListener("input", (event) => updateForm(null, true));
fmtWordlistRadio   .addEventListener("input", (event) => updateForm(null, true));
fmtInoutRadio      .addEventListener("input", (event) => updateForm(null, true));
fmtInoutPreserveRadio.addEventListener("input", (event) => updateForm(null, true));
fmtMdfRadio        .addEventListener("input", (event) => updateForm(null, true));
fmtMdfEtymRadio    .addEventListener("input", (event) => updateForm(null, true));

highlightUnused.addEventListener("click", () => {
    if (highlightUnused.checked) {
        const rules = rulesEditor.state.doc.toString();
        const words = wordsArea.value;
        const sep = '/';  // irrelevant here, really
        const inputFormat = form.elements['inputFormat'].value;
        applyChanges(rules, words, sep, 'ReportNotApplied', inputFormat, 'NoHighlight', 'WordsOnlyOutput', timeout);
    } else {
        highlights = [];
    }
});


function highlightResult(newHighlights) {
    highlights = newHighlights;
    forceLinterUpdate();
}


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

    errors = []; highlights = [];

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
