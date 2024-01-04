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

form.addEventListener("submit", (event) => {
    event.preventDefault();

    const data = new FormData(form);
    const paradigm = data.get("paradigm");
    const roots = data.get("roots");
    const separateLines = data.get("separateLines");

    const output = buildParadigm(paradigm, roots, separateLines);
    console.log(output);
    document.getElementById("output").innerHTML = output;
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
