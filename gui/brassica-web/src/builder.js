import Split from "split.js";

import {hs, withBytesPtr, decodeStableCStringLen, encoder, decoder} from "./interop.js";

/***********************
 * Haskell interop     *
 ***********************/

function buildParadigm(paradigm, words, separateLines) {
    const inputParadigm = encoder.encode(paradigm);
    const inputWords = encoder.encode(words);

    const separateLinesC = separateLines ? 1 : 0;

    var output = "";
    withBytesPtr(inputParadigm, (inputParadigmPtr, inputParadigmLen) => {
        withBytesPtr(inputWords, (inputWordsPtr, inputWordsLen) => {
            try {
                const outputStableCStringLen = hs.parseAndBuildParadigm_hs(
                    inputParadigmPtr, inputParadigmLen,
                    inputWordsPtr, inputWordsLen,
                    separateLinesC);
                output = decodeStableCStringLen(outputStableCStringLen);
            } catch (err) {
                console.error(err);
                output = err;
            }
        });
    });
    return output;
}

/***********************
 * Set up content      *
 ***********************/

Split(["#paradigm-div", "#roots-div", "#output-div"]);

const form = document.getElementById("brassica-form");
const outputDiv = document.getElementById("output");

form.addEventListener("submit", (event) => {
    event.preventDefault();

    const data = new FormData(form);
    const paradigm = data.get("paradigm");
    const roots = data.get("roots");
    const separateLines = data.get("separateLines");

    const output = buildParadigm(paradigm, roots, separateLines);
    console.log(output);
    outputDiv.innerHTML = output;
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

const paradigmArea = document.getElementById("paradigm");
const rootsArea = document.getElementById("roots");

document.getElementById("download-paradigm").addEventListener("click", (event) => {
    event.preventDefault();
    save("paradigm.bsc", paradigmArea.value);
});
document.getElementById("download-roots").addEventListener("click", (event) => {
    event.preventDefault();
    save("roots.lex", rootsArea.value);
});

const inputFileParadigm = document.getElementById("input-file-paradigm");
inputFileParadigm.addEventListener("change", (event) => {
    const file = inputFileParadigm.files[0];
    if (file) {
        const reader = new FileReader();
        reader.onload = (e) => {
            paradigmArea.value = e.target.result;
        };
        reader.readAsText(file);
    }
});
document
    .getElementById("open-paradigm")
    .addEventListener("click", (event) => inputFileParadigm.click());

const inputFileRoots = document.getElementById("input-file-roots");
inputFileRoots.addEventListener("change", (event) => {
    const file = inputFileRoots.files[0];
    if (file) {
        const reader = new FileReader();
        reader.onload = (e) => {
            rootsArea.value = e.target.result;
        };
        reader.readAsText(file);
    }
});
document
    .getElementById("open-roots")
    .addEventListener("click", (event) => inputFileRoots.click());

document.getElementById("select-all-paradigm").addEventListener("click", (event) => {
    paradigmArea.select();
});
document.getElementById("select-all-roots").addEventListener("click", (event) => {
    rootsArea.select();
});
document.getElementById("select-all-output").addEventListener("click", (event) => {
    // see https://stackoverflow.com/a/1173319
    var range = document.createRange();
    range.selectNode(outputDiv);
    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);
});
