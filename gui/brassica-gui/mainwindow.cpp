#include "mainwindow.h"
#include "finddialog.h"
#include "paradigmwindow.h"
#include "settingsdialog.h"

#include <QFileDialog>
#include <QGridLayout>
#include <QGroupBox>
#include <QJsonValue>
#include <QLabel>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QRadioButton>
#include <QScrollBar>
#include <QShortcut>
#include <QTextCodec>
#include <QTextStream>
#include <Qt>
#include <qkeysequence.h>
#include <qnamespace.h>
#include <qradiobutton.h>

MainWindow::MainWindow(BrassicaProcess *proc, QWidget *parent)
    : QMainWindow(parent)
    , settings()
    , proc(proc)
{
    prev = new QJsonValue(QJsonValue::Null);

    setWindowTitle("Brassica");

    QWidget *window = new QWidget;
    setCentralWidget(window);

    setupWidgets(window);
    setupMenuBar();
    applySettings();

    connect(applyBtn      , &QPushButton::clicked  , this, [this] { applySoundChanges(false, false); });
    connect(reportRulesBtn, &QPushButton::clicked  , this, [this] { applySoundChanges(false, true); } );

    QShortcut *applyShortcut1 = new QShortcut(QKeySequence(Qt::CTRL | Qt::Key_Return), this);
    QShortcut *applyShortcut2 = new QShortcut(QKeySequence(Qt::CTRL | Qt::Key_Enter ), this);
    connect(applyShortcut1, &QShortcut::activated, this, [this] { applySoundChanges(false, false); });
    connect(applyShortcut2, &QShortcut::activated, this, [this] { applySoundChanges(false, false); });

    QShortcut *toggleShortcut = new QShortcut(QKeySequence(Qt::CTRL | Qt::Key_Tab), this);
    connect(toggleShortcut, &QShortcut::activated, this, &MainWindow::toggleCursor);

    connect(rulesEdit, &QPlainTextEdit::textChanged, this, &MainWindow::rulesModified);
    connect(wordsEdit, &QPlainTextEdit::textChanged, this, &MainWindow::lexiconModified);

    connect(rulesEdit, &QPlainTextEdit::textChanged, this, &MainWindow::reparseCategories);

    connect(wordsEditVScroll , &QAbstractSlider::valueChanged, this, &MainWindow::updateOutputFromWordsSlider);
    connect(outputEditVScroll, &QAbstractSlider::valueChanged, this, &MainWindow::updateWordsFromOutputSlider);
    connect(synchroniseScrolls, &QCheckBox::toggled, this, [this](bool checked) {
        if (checked) updateOutputFromWordsSlider(wordsEditVScroll->value());
    });

    connect(mdfBtn   , &QRadioButton::toggled, this, &MainWindow::reselectCheckboxes);
    connect(mdfAltBtn, &QRadioButton::toggled, this, &MainWindow::reselectCheckboxes);

    // live highlighting
    connect(rulesEdit, &QPlainTextEdit::textChanged, this, [this] { applySoundChanges(true, false); });
    connect(wordsEdit, &QPlainTextEdit::textChanged, this, [this] { applySoundChanges(true, false); });
    connect(nohighlightBtn   , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(diffhighlightBtn , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(inputhighlightBtn, &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(rawBtn           , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(mdfBtn           , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(mdfAltBtn        , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(mdfoutBtn        , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(mdfetymoutBtn    , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(rawoutBtn        , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(inoutBtn         , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });
    connect(inoutBtnPreserve , &QRadioButton::toggled, this, [this] { applySoundChanges(true, false); });

    connect(viewLive, &QRadioButton::toggled, this, [this](bool checked) { if (checked) applySoundChanges(false, false); });
}

MainWindow::~MainWindow()
{
    delete prev;
    // proc is deleted in main()
}

void MainWindow::setupWidgets(QWidget *central)
{
    QGridLayout *mainLayout = new QGridLayout(central);

    QSplitter *mainSplitter = new QSplitter(Qt::Horizontal);
    mainLayout->addWidget(mainSplitter);

    QVBoxLayout *rulesLayout      = mkLayoutWithContainer(mainSplitter);
    QVBoxLayout *wordsLayout      = mkLayoutWithContainer(mainSplitter);
    QVBoxLayout *midLayout        = mkLayoutWithContainer(mainSplitter);
    QVBoxLayout *outputLayout     = mkLayoutWithContainer(mainSplitter);

    midLayout->setAlignment(Qt::AlignTop);

    QLabel *rulesLbl = new QLabel("Rules:");
    rulesEdit = new QPlainTextEdit;
    rulesHl = new RulesHighlighter(rulesEdit->document());
    rulesLayout->addWidget(rulesLbl);
    rulesLayout->addWidget(rulesEdit);

    QLabel *wordsLbl = new QLabel("Input lexicon:");
    wordsEdit = new QPlainTextEdit;
    wordsLayout->addWidget(wordsLbl);
    wordsLayout->addWidget(wordsEdit);

    wordsEditVScroll = wordsEdit->verticalScrollBar();

    applyBtn = new QPushButton("Apply");
    midLayout->addWidget(applyBtn);

    reportRulesBtn = new QPushButton("Report rules applied");
    midLayout->addWidget(reportRulesBtn);

    QGroupBox *highlightBox = new QGroupBox("Output highlighting");
    QVBoxLayout *highlightLayout = new QVBoxLayout(highlightBox);
    midLayout->addWidget(highlightBox);

    nohighlightBtn = new QRadioButton("No highlighting");
    nohighlightBtn->setChecked(true);
    highlightLayout->addWidget(nohighlightBtn);

    diffhighlightBtn = new QRadioButton("Different to last run");
    highlightLayout->addWidget(diffhighlightBtn);

    inputhighlightBtn = new QRadioButton("Any rule applied");
    highlightLayout->addWidget(inputhighlightBtn);

    QGroupBox *inputFormatBox = new QGroupBox("Input lexicon format");
    QVBoxLayout *inputFormatLayout = new QVBoxLayout(inputFormatBox);
    midLayout->addWidget(inputFormatBox);

    rawBtn = new QRadioButton("Wordlist + glosses");
    rawBtn->setChecked(true);
    inputFormatLayout->addWidget(rawBtn);

    mdfBtn = new QRadioButton("MDF file (standard hierarchy)");
    inputFormatLayout->addWidget(mdfBtn);

    mdfAltBtn = new QRadioButton("MDF file (alternate hierarchy)");
    inputFormatLayout->addWidget(mdfAltBtn);

    outputFormatBox = new QGroupBox("Output format");
    QVBoxLayout *outputFormatLayout = new QVBoxLayout(outputFormatBox);
    midLayout->addWidget(outputFormatBox);

    rawoutBtn = new QRadioButton("Wordlist");
    rawoutBtn->setChecked(true);
    outputFormatLayout->addWidget(rawoutBtn);

    inoutBtn = new QRadioButton("Input→output");
    outputFormatLayout->addWidget(inoutBtn);

    inoutBtnPreserve = new QRadioButton("Input→output + glosses");
    outputFormatLayout->addWidget(inoutBtnPreserve);

    mdfoutBtn = new QRadioButton("MDF output");
    mdfoutBtn->setEnabled(false);
    outputFormatLayout->addWidget(mdfoutBtn);

    mdfetymoutBtn = new QRadioButton("MDF output with etymologies");
    mdfetymoutBtn->setEnabled(false);
    outputFormatLayout->addWidget(mdfetymoutBtn);

    viewLive = new QCheckBox("View results live");
    midLayout->addWidget(viewLive);

    synchroniseScrolls = new QCheckBox("Synchronise scroll positions");
    synchroniseScrolls->setChecked(true);
    midLayout->addWidget(synchroniseScrolls);

    QHBoxLayout *multiResultLayout = new QHBoxLayout();
    multiResultLayout->setAlignment(Qt::AlignLeft);
    midLayout->addLayout(multiResultLayout);

    QLabel *multiResultLabel = new QLabel("Multiple result separator:");
    multiResultLayout->addWidget(multiResultLabel);

    multiResultSep = new QLineEdit("/");
    multiResultSep->setMaximumSize(
        100,
        multiResultSep->maximumSize().height());
    multiResultLayout->addWidget(multiResultSep);

    multiResultLayout->addStretch();

    QLabel *outputLbl = new QLabel("Output lexicon:");
    outputEdit = new QTextEdit;
    outputEdit->setReadOnly(true);
    outputLayout->addWidget(outputLbl);
    outputLayout->addWidget(outputEdit);

    outputEditVScroll = outputEdit->verticalScrollBar();
}

#define DOTOFOCUS(FN) [this] { \
    QPlainTextEdit *f = dynamic_cast<QPlainTextEdit*>(focusWidget()); \
    if (f != nullptr) f->FN(); \
}

void MainWindow::setupMenuBar()
{
    QMenu *fileMenu = menuBar()->addMenu("&File");
    fileMenu->addAction("Open rules", QKeySequence(Qt::CTRL | Qt::Key_O), this, &MainWindow::openRules);
    fileMenu->addAction("Save rules", QKeySequence(Qt::CTRL | Qt::Key_S), this, &MainWindow::saveRules);
    fileMenu->addAction("Save rules as", this, &MainWindow::saveRulesAs);
    fileMenu->addAction("Open lexicon", QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_O), this, &MainWindow::openLexicon);
    fileMenu->addAction("Save lexicon", QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_S), this, &MainWindow::saveLexicon);
    fileMenu->addAction("Save lexicon as", this, &MainWindow::saveLexiconAs);

    QMenu *editMenu = menuBar()->addMenu("&Edit");
    editMenu->addAction("Undo", QKeySequence::Undo, this, DOTOFOCUS(undo));
    editMenu->addAction("Redo", QKeySequence::Redo, this, DOTOFOCUS(redo));
    editMenu->addSeparator();
    editMenu->addAction("Cut", QKeySequence::Cut, this, DOTOFOCUS(cut));
    editMenu->addAction("Copy", QKeySequence::Copy, this, DOTOFOCUS(copy));
    editMenu->addAction("Paste", QKeySequence::Paste, this, DOTOFOCUS(paste));
    editMenu->addSeparator();
    editMenu->addAction("Find", QKeySequence::Find, this, &MainWindow::showFindWindow);

    QMenu *toolsMenu = menuBar()->addMenu("&Tools");
    toolsMenu->addAction("Paradigm builder", this, &MainWindow::showParadigmBuilder);
    toolsMenu->addAction("Options...", this, &MainWindow::editSettings);
}

QVBoxLayout *MainWindow::mkLayoutWithContainer(QSplitter *splitter)
{
    QWidget *container = new QWidget;
    QVBoxLayout *layout = new QVBoxLayout(container);
    layout->setContentsMargins(0,0,0,0);
    splitter->addWidget(container);
    return layout;
}

void MainWindow::doSaveRules(QString fileName)
{
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString rules = rulesEdit->toPlainText();
    file.write(rules.toUtf8());
    currentRulesFile = fileName;
    rulesDirty = false;
    rulesEdit->document()->setModified(false);
    refreshTitle();
}

void MainWindow::doSaveLexicon(QString fileName)
{
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString lexicon = wordsEdit->toPlainText();
    file.write(lexicon.toUtf8());
    currentLexiconFile = fileName;
    lexiconDirty = false;
    wordsEdit->document()->setModified(false);
    refreshTitle();
}

bool MainWindow::checkRulesDirty()
{
    if (rulesDirty) {
        const QMessageBox::StandardButton ret =
            QMessageBox::warning(this, "Brassica",
                                 "The sound changes have been modified.\nDo you want to save your changes?",
                                 QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
        switch (ret) {
        case QMessageBox::Save:
            saveRules(); break;
        case QMessageBox::Cancel:
            return true;
        default:
            break;
        }
    }

    return false;
}

bool MainWindow::checkLexiconDirty()
{
    if (lexiconDirty) {
        const QMessageBox::StandardButton ret =
            QMessageBox::warning(this, "Brassica",
                                 "The lexicon has been modified.\nDo you want to save your changes?",
                                 QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel);
        switch (ret) {
        case QMessageBox::Save:
            saveLexicon(); break;
        case QMessageBox::Cancel:
            return true;
        default:
            break;
        }
    }

    return false;
}

void MainWindow::refreshTitle()
{
    // can't use setWindowModified here because we have
    // two separate modified states, so can't use Qt's [*] placeholder

    QString openFiles;

    if (!currentRulesFile.isEmpty()) {
        QFileInfo rulesInfo(currentRulesFile);
        openFiles = rulesInfo.fileName();
        if (rulesDirty) openFiles += '*';
    }

    if (!currentLexiconFile.isEmpty()) {
        QFileInfo lexiconInfo(currentLexiconFile);
        if (!openFiles.isEmpty()) openFiles += ", ";
        openFiles += lexiconInfo.fileName();
        if (lexiconDirty) openFiles += '*';
    }

    QString windowTitle("Brassica");
    if (!openFiles.isEmpty()) {
        windowTitle += " - ";
        windowTitle += openFiles;
    }

    setWindowTitle(windowTitle);
}

void MainWindow::applySettings()
{
    rulesEdit->setFont(settings.rulesFont);
    wordsEdit->setFont(settings.wordsFont);
    outputEdit->setFont(settings.wordsFont);
    settings.writeSettings();
}

void MainWindow::closeEvent(QCloseEvent *event)
{
    if (checkRulesDirty()) { event->ignore(); return; }
    if (checkLexiconDirty()) { event->ignore(); return; }
    event->accept();
}

void MainWindow::applySoundChanges(bool live, bool reportRules)
{
    if (live && !viewLive->isChecked()) return;

    QString rules      = rulesEdit     ->toPlainText();
    QString words      = wordsEdit     ->toPlainText();

    //QString output = proc->applyRules(categories, rules, words);

    BrassicaProcess::InputLexiconFormat infmt = BrassicaProcess::Raw;
    if (mdfBtn->isChecked()) infmt = BrassicaProcess::MDFStandard;
    else if (mdfAltBtn->isChecked()) infmt = BrassicaProcess::MDFAlternate;

    BrassicaProcess::HighlightMode checkedHl = BrassicaProcess::NoHighlight;
    if (diffhighlightBtn->isChecked()) checkedHl = BrassicaProcess::DifferentToLastRun;
    else if (inputhighlightBtn->isChecked()) checkedHl = BrassicaProcess::DifferentToInput;

    BrassicaProcess::OutputMode outMode = BrassicaProcess::WordsOnlyOutput;
    if (mdfoutBtn->isChecked()) outMode = BrassicaProcess::MDFOutput;
    else if (mdfetymoutBtn->isChecked()) outMode = BrassicaProcess::MDFOutputWithEtymons;
    else if (inoutBtn->isChecked()) outMode = BrassicaProcess::WordsWithProtoOutput;
    else if (inoutBtnPreserve->isChecked()) outMode = BrassicaProcess::WordsWithProtoOutputPreserve;

    QString output = proc->parseTokeniseAndApplyRules(
        rules,
        words,
        reportRules,
        infmt,
        checkedHl,
        outMode,
        prev,
        multiResultSep->text());

    blockScrollTrackingEvent = true;
    outputEdit->setHtml("<pre>" + output + "</pre>");

    blockScrollTrackingEvent = false;
    updateOutputFromWordsSlider(wordsEditVScroll->value());
}

void MainWindow::openRules()
{
    if (checkRulesDirty()) return;

    QString fileName = QFileDialog::getOpenFileName(this, "Open rules", QString(), "Brassica rules (*.bsc);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    rulesEdit->setPlainText(QString::fromUtf8(file.readAll()));
    currentRulesFile = fileName;
    rulesDirty = false;
    refreshTitle();
}

void MainWindow::saveRules()
{
    if (currentRulesFile.isEmpty()) saveRulesAs();
    else doSaveRules(currentRulesFile);
}

void MainWindow::saveRulesAs()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Save rules", QString(), "Brassica rules (*.bsc);;All files (*.*)");
    doSaveRules(fileName);
}

void MainWindow::openLexicon()
{
    if (checkLexiconDirty()) return;

    QString fileName = QFileDialog::getOpenFileName(this, "Open lexicon", QString(), "Lexicon files (*.lex);;MDF files (*.mdf *.txt);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    wordsEdit->setPlainText(QString::fromUtf8(file.readAll()));

    QString extension = QFileInfo(fileName).suffix();
    if (extension == "mdf" || extension == "txt")
        mdfBtn->setChecked(true);
    else
        rawBtn->setChecked(true);

    currentLexiconFile = fileName;
    lexiconDirty = false;
    refreshTitle();
}

void MainWindow::saveLexicon()
{
    if (currentLexiconFile.isEmpty()) saveLexiconAs();
    else doSaveLexicon(currentLexiconFile);
}

void MainWindow::saveLexiconAs()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Open lexicon", QString(), "Lexicon files (*.lex);;MDF files (*.mdf *.txt);;All files (*.*)");
    doSaveLexicon(fileName);
}


void MainWindow::toggleCursor()
{
    if (rulesEdit->hasFocus()) { wordsEdit->setFocus(Qt::TabFocusReason); return; }
    if (wordsEdit->hasFocus()) { rulesEdit->setFocus(Qt::TabFocusReason); return; }
}

void MainWindow::rulesModified()
{
    rulesDirty = rulesEdit->document()->isModified();
    refreshTitle();
}

void MainWindow::lexiconModified()
{
    lexiconDirty = wordsEdit->document()->isModified();
    refreshTitle();
}

void MainWindow::updateOutputFromWordsSlider(int value)
{
    if (!synchroniseScrolls->isChecked()) return;

    if (blockScrollTrackingEvent) {
        blockScrollTrackingEvent = false;
    } else {
        float wordsMin = wordsEditVScroll->minimum();
        float wordsMax = wordsEditVScroll->maximum();
        float outputMin = outputEditVScroll->minimum();
        float outputMax = outputEditVScroll->maximum();

        float ratio = (value - wordsMin) / (wordsMax - wordsMin);
        blockScrollTrackingEvent = true;
        outputEditVScroll->setValue(
            (ratio * (outputMax - outputMin)) + outputMin);
    }
}

void MainWindow::updateWordsFromOutputSlider(int value)
{
    if (!synchroniseScrolls->isChecked()) return;

    if (blockScrollTrackingEvent) {
        blockScrollTrackingEvent = false;
    } else {
        float wordsMin = wordsEditVScroll->minimum();
        float wordsMax = wordsEditVScroll->maximum();
        float outputMin = outputEditVScroll->minimum();
        float outputMax = outputEditVScroll->maximum();

        float ratio = (value - outputMin) / (outputMax - outputMin);
        blockScrollTrackingEvent = true;
        wordsEditVScroll->setValue(
            (ratio * (wordsMax - wordsMin)) + wordsMin);
    }
}

void MainWindow::reselectCheckboxes()
{
    if (mdfBtn->isChecked() || mdfAltBtn->isChecked()) {
        mdfoutBtn->setEnabled(true);
        mdfetymoutBtn->setEnabled(true);
    } else {
        if (mdfoutBtn->isChecked() || mdfetymoutBtn->isChecked())
            rawoutBtn->setChecked(true);
        mdfoutBtn->setEnabled(false);
        mdfetymoutBtn->setEnabled(false);
    }
}

void MainWindow::showFindWindow() {
    if (!findDialog) {
        FindDialog::FindArea area;
        if (rulesEdit->hasFocus()) area = FindDialog::Changes;
        else if (wordsEdit->hasFocus()) area = FindDialog::Input;
        else area = FindDialog::Output;

        findDialog = new FindDialog(area, this);
        connect(findDialog, &FindDialog::findNext, this, &MainWindow::findNext);
    }

    findDialog->show();
    findDialog->raise();
    findDialog->activateWindow();
}

void MainWindow::findNext(QString substring,
                          FindDialog::FindArea area,
                          QTextDocument::FindFlags flags) {
    QTextCursor cursor;
    switch (area) {
    case FindDialog::Changes: cursor = rulesEdit->textCursor(); break;
    case FindDialog::Input:   cursor = wordsEdit->textCursor(); break;
    case FindDialog::Output:  cursor = outputEdit->textCursor(); break;
    }

    QTextDocument *doc = cursor.document();
    QTextCursor found = doc->find(substring, cursor, flags);
    if (!found.isNull()) {
        switch (area) {
        case FindDialog::Changes: rulesEdit->setTextCursor(found); break;
        case FindDialog::Input:   wordsEdit->setTextCursor(found); break;
        case FindDialog::Output:  outputEdit->setTextCursor(found); break;
        }
    }
}


void MainWindow::showParadigmBuilder()
{
    ParadigmWindow *pw = new ParadigmWindow(proc, this);
    pw->show();
}

void MainWindow::reparseCategories()
{
    QString changesText = rulesEdit->toPlainText();

    /* A very simple state machine. Starts recording categories when
     * `categories` is read. After this, split each line at the equals sign and
     * add to `categories`. When `end' is reached, read to next `categories`.
     * Feature lines need their own processing.
     */

    QStringList categories;

    {
        QTextStream stream;
        stream.setString(&changesText, QIODevice::ReadOnly);
        QString line;
        QStringList lineParts;
        bool inCategories;
        while(stream.readLineInto(&line))
        {
            if (line.contains("categories")) inCategories = true;
            else if (line == "end") inCategories = false;
            else if (line.contains("feature"))
            {
                static const QRegularExpression featureRegex("=|/|feature");
                lineParts = line.split(featureRegex);
                // every second part from the end is a category name,
                for (int i = lineParts.length()-2; i>=0; i-=2)
                    categories.append(lineParts[i].trimmed());
            }
            else if (inCategories)
            {
                lineParts = line.split('=');
                if (lineParts.length() > 1)
                    categories.append(lineParts[0].trimmed());
            }
        }
    }

    {
        const QSignalBlocker blocker(rulesEdit);
        rulesHl->setCategories(categories);
    }
}

void MainWindow::editSettings()
{
    SettingsDialog dlg(settings, this);
    if (dlg.exec() == QDialog::Accepted)
        applySettings();
}

