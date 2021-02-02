function setupMode(cats) {
    var regexps = [
        {regex: /_|>|#/, token: 'special'},
        {regex: /\//, token: 'separator'},
        {regex: /\[.*?\]/, token: 'category'},
        {regex: /\*.*/, token: 'comment'}
    ];
    if (cats.length > 0) {
        regexps.push({regex: new RegExp(cats.sort(function(a,b){return b.length-a.length}).join('|')), token: 'category'});
    }
    CodeMirror.defineSimpleMode('soundchange', { start: regexps });
    if (rulesCodeMirror) rulesCodeMirror.setOption('mode', 'soundchange');  // refresh mode
}

setupMode([]);

var rulesCodeMirror = CodeMirror.fromTextArea(document.getElementById('rules'), {
    mode: 'soundchange',
    smartIndent: false,
    lineWrapping: true
});
