#include "BrassicaInterop_stub.h"
#include "mainwindow.h"
#include "paradigmwindow.h"
#include "settingsdialog.h"

#include <QFileDialog>
#include <QGridLayout>
#include <QGroupBox>
#include <QLabel>
#include <QMenu>
#include <QMenuBar>
#include <QRadioButton>
#include <QTextCodec>
#include <QTextStream>
#include <Qt>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , settings()
{
    setWindowTitle("Brassica");

    hsResults = initResults();

    QWidget *window = new QWidget;
    setCentralWidget(window);

    setupWidgets(window);
    setupMenuBar();
    applySettings();

    connect(applyBtn      , &QPushButton::clicked  , [this] { applySoundChanges(false, false); });
    connect(reportRulesBtn, &QPushButton::clicked  , [this] { applySoundChanges(false, true); } );
    connect(rulesEdit, &QPlainTextEdit::textChanged, [this] { applySoundChanges(true, false); });
    connect(wordsEdit, &QPlainTextEdit::textChanged, [this] { applySoundChanges(true, false); });

    connect(rulesEdit, &QPlainTextEdit::textChanged, this, &MainWindow::reparseCategories);

    connect(mdfBtn, &QRadioButton::toggled, [this](bool checked) {
        if (checked) {
            mdfoutBtn->setEnabled(true);
            mdfetymoutBtn->setEnabled(true);
        } else {
            if (mdfoutBtn->isChecked() || mdfetymoutBtn->isChecked())
                rawoutBtn->setChecked(true);
            mdfoutBtn->setEnabled(false);
            mdfetymoutBtn->setEnabled(false);
        }
    });
}

MainWindow::~MainWindow()
{
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

    inputhighlightBtn = new QRadioButton("Different to input");
    highlightLayout->addWidget(inputhighlightBtn);

    QGroupBox *inputFormatBox = new QGroupBox("Input lexicon format");
    QVBoxLayout *inputFormatLayout = new QVBoxLayout(inputFormatBox);
    midLayout->addWidget(inputFormatBox);

    rawBtn = new QRadioButton("Wordlist + glosses");
    rawBtn->setChecked(true);
    inputFormatLayout->addWidget(rawBtn);

    mdfBtn = new QRadioButton("MDF file");
    inputFormatLayout->addWidget(mdfBtn);

    outputFormatBox = new QGroupBox("Output format");
    QVBoxLayout *outputFormatLayout = new QVBoxLayout(outputFormatBox);
    midLayout->addWidget(outputFormatBox);

    rawoutBtn = new QRadioButton("Wordlist");
    rawoutBtn->setChecked(true);
    outputFormatLayout->addWidget(rawoutBtn);

    inoutBtn = new QRadioButton("Input→output");
    outputFormatLayout->addWidget(inoutBtn);

    mdfoutBtn = new QRadioButton("MDF output");
    mdfoutBtn->setEnabled(false);
    outputFormatLayout->addWidget(mdfoutBtn);

    mdfetymoutBtn = new QRadioButton("MDF output with etymologies");
    mdfetymoutBtn->setEnabled(false);
    outputFormatLayout->addWidget(mdfetymoutBtn);

    viewLive = new QCheckBox("View results live");
    midLayout->addWidget(viewLive);

    QLabel *outputLbl = new QLabel("Output lexicon:");
    outputEdit = new QTextEdit;
    outputEdit->setReadOnly(true);
    outputLayout->addWidget(outputLbl);
    outputLayout->addWidget(outputEdit);
}

void MainWindow::setupMenuBar()
{
    QMenu *fileMenu = menuBar()->addMenu("&File");
    fileMenu->addAction("Open rules", this, &MainWindow::openRules, QKeySequence(Qt::CTRL | Qt::Key_O));
    fileMenu->addAction("Save rules", this, &MainWindow::saveRules, QKeySequence(Qt::CTRL | Qt::Key_S));
    fileMenu->addAction("Save rules as", this, &MainWindow::saveRulesAs);
    fileMenu->addAction("Open lexicon", this, &MainWindow::openLexicon, QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_O));
    fileMenu->addAction("Save lexicon", this, &MainWindow::saveLexicon, QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_S));
    fileMenu->addAction("Save lexicon as", this, &MainWindow::saveLexiconAs);

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
}

void MainWindow::doSaveLexicon(QString fileName)
{
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString lexicon = wordsEdit->toPlainText();
    file.write(lexicon.toUtf8());
}

void MainWindow::applySettings()
{
    rulesEdit->setFont(settings.rulesFont);
    wordsEdit->setFont(settings.wordsFont);
    outputEdit->setFont(settings.wordsFont);
    settings.writeSettings();
}

void MainWindow::applySoundChanges(bool live, bool reportRules)
{
    if (live && !viewLive->isChecked()) return;

    QString rules      = rulesEdit     ->toPlainText();
    QString words      = wordsEdit     ->toPlainText();

    //QString output = proc->applyRules(categories, rules, words);

    int infmt = 0;
    if (mdfBtn->isChecked()) infmt = 1;

    int checkedHl = 0;
    if (diffhighlightBtn->isChecked()) checkedHl = 1;
    else if (inputhighlightBtn->isChecked()) checkedHl = 2;

    int outMode = 1; // default to wordlist / rawoutBtn
    if (mdfoutBtn->isChecked()) outMode = 0;
    else if (mdfetymoutBtn->isChecked()) outMode = 2;
    else if (inoutBtn->isChecked()) outMode = 3;

    QByteArray output = QByteArray((char*) parseTokeniseAndApplyRules_hs(
                                       rules.toUtf8().data(),
                                       words.toUtf8().data(),
                                       reportRules,
                                       infmt,
                                       checkedHl,
                                       outMode,
                                       hsResults));
    outputEdit->setHtml(QString::fromUtf8(output));
}

void MainWindow::openRules()
{
    QString fileName = QFileDialog::getOpenFileName(this, "Open rules", QString(), "Brassica rules (*.bsc);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    rulesEdit->setPlainText(QString::fromUtf8(file.readAll()));
    currentRulesFile = fileName;
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
    currentRulesFile = fileName;
}

void MainWindow::openLexicon()
{
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
    currentLexiconFile = fileName;
}

void MainWindow::showParadigmBuilder()
{
    ParadigmWindow *pw = new ParadigmWindow(this);
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
                lineParts = line.split(QRegularExpression("=|/|feature"));
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

