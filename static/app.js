const { Menu, dialog, getCurrentWindow } = require('electron').remote;

const template = [
    {
        label: 'File',
        submenu: [
            {
                label: 'Open rules',
                click: () => {
                    let path = dialog.showOpenDialogSync(getCurrentWindow(), {
                        filters: [
                            { name: 'Sound Change Files', extensions: ['bsc'] },
                            { name: 'All Files', extensions: ['*'] }
                        ]
                    });
                    window.hs.openRules(path);
                }
            },
            {
                label: 'Save rules',
                click: () => {
                    let path = dialog.showSaveDialogSync(getCurrentWindow(), {
                        filters: [
                            { name: 'Sound Change Files', extensions: ['bsc'] },
                            { name: 'All Files', extensions: ['*'] }
                        ]
                    });
                    window.hs.saveRules(path);
                }
            },
            {
                label: 'Open lexicon',
                click: () => {
                    let path = dialog.showOpenDialogSync(getCurrentWindow(), {
                        filters: [
                            { name: 'Lexicon Files', extensions: ['lex'] },
                            { name: 'All Files', extensions: ['*'] }
                        ]
                    });
                    window.hs.openLexicon(path);
                }
            },
            {
                label: 'Save lexicon',
                click: () => {
                    let path = dialog.showSaveDialogSync(getCurrentWindow(), {
                        filters: [
                            { name: 'Lexicon Files', extensions: ['lex'] },
                            { name: 'All Files', extensions: ['*'] }
                        ]
                    });
                    window.hs.saveLexicon(path);
                }
            }
        ]
    },
    { role: 'viewMenu' }
];

const menu = Menu.buildFromTemplate(template);
Menu.setApplicationMenu(menu);
