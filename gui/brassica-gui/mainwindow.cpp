#include "BrassicaInterop_stub.h"
#include "mainwindow.h"
#include "paradigmwindow.h"

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
{
    setWindowTitle("Brassica");

    hsResults = initResults();

    QWidget *window = new QWidget;
    setCentralWidget(window);

    setupWidgets(window);
    setupMenuBar();

    connect(applyBtn , &QPushButton::clicked       , [this] { applySoundChanges(false); });
    connect(rulesEdit, &QPlainTextEdit::textChanged, [this] { applySoundChanges(true); });
    connect(wordsEdit, &QPlainTextEdit::textChanged, [this] { applySoundChanges(true); });

    connect(reportRulesBtn, &QPushButton::clicked, this, &MainWindow::reportRulesApplied);
    connect(rulesEdit, &QPlainTextEdit::textChanged, this, &MainWindow::reparseCategories);
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

    const QFont fixedFont = QFontDatabase::systemFont(QFontDatabase::FixedFont);
    const QFont textFont = QFont("Microsoft Sans Serif");

    QLabel *rulesLbl = new QLabel("Rules:");
    rulesEdit = new QPlainTextEdit;
    rulesEdit->setFont(fixedFont);
    rulesHl = new RulesHighlighter(rulesEdit->document());
    rulesLayout->addWidget(rulesLbl);
    rulesLayout->addWidget(rulesEdit);

    QLabel *wordsLbl = new QLabel("Input lexicon:");
    wordsEdit = new QPlainTextEdit;
    wordsEdit->setFont(textFont);
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

    viewLive = new QCheckBox("View results live");
    midLayout->addWidget(viewLive);

    QLabel *outputLbl = new QLabel("Output lexicon:");
    outputEdit = new QTextEdit;
    outputEdit->setReadOnly(true);
    outputEdit->setFont(textFont);
    outputLayout->addWidget(outputLbl);
    outputLayout->addWidget(outputEdit);
}

void MainWindow::setupMenuBar()
{
    QMenu *fileMenu = menuBar()->addMenu("&File");
    fileMenu->addAction("Open rules", this, &MainWindow::openRules, QKeySequence::Open);
    fileMenu->addAction("Save rules", this, &MainWindow::saveRules, QKeySequence::Save);
    fileMenu->addAction("Open lexicon", this, &MainWindow::openLexicon);
    fileMenu->addAction("Save lexicon", this, &MainWindow::saveLexicon);

    QMenu *toolsMenu = menuBar()->addMenu("&Tools");
    toolsMenu->addAction("Paradigm builder", this, &MainWindow::showParadigmBuilder);
}

QVBoxLayout *MainWindow::mkLayoutWithContainer(QSplitter *splitter)
{
    QWidget *container = new QWidget;
    QVBoxLayout *layout = new QVBoxLayout(container);
    layout->setContentsMargins(0,0,0,0);
    splitter->addWidget(container);
    return layout;
}

void MainWindow::applySoundChanges(bool live)
{
    if (live && !viewLive->isChecked()) return;

    QString rules      = rulesEdit     ->toPlainText();
    QString words      = wordsEdit     ->toPlainText();

    //QString output = proc->applyRules(categories, rules, words);

    int checkedHl = 0;
    if (diffhighlightBtn->isChecked()) checkedHl = 1;
    else if (inputhighlightBtn->isChecked()) checkedHl = 2;

    QByteArray output = QByteArray((char*) parseTokeniseAndApplyRules_hs(
                                       rules.toUtf8().data(),
                                       words.toUtf8().data(),
                                       false,
                                       checkedHl,
                                       hsResults));
    outputEdit->setHtml(QString::fromUtf8(output));
}

void MainWindow::reportRulesApplied()
{
    QString rules      = rulesEdit     ->toPlainText();
    QString words      = wordsEdit     ->toPlainText();

    //QString output = proc->applyRules(categories, rules, words);

    QByteArray output = QByteArray((char*) parseTokeniseAndApplyRules_hs(
                                       rules.toUtf8().data(),
                                       words.toUtf8().data(),
                                       true,
                                       0,
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
}

void MainWindow::saveRules()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Save rules", QString(), "Brassica rules (*.bsc);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString rules = rulesEdit->toPlainText();

    file.write(rules.toUtf8());
}

void MainWindow::openLexicon()
{
    QString fileName = QFileDialog::getOpenFileName(this, "Open lexicon", QString(), "Lexicon files (*.lex);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    wordsEdit->setPlainText(QString::fromUtf8(file.readAll()));
}

void MainWindow::saveLexicon()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Open lexicon", QString(), "Lexicon files (*.lex);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString lexicon = wordsEdit->toPlainText();
    file.write(lexicon.toUtf8());
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
            else if (inCategories)
            {
                lineParts = line.split('=');
                if (lineParts.length() > 1)
                    categories.append(lineParts[0].trimmed());
            }
        }
    }

    rulesHl->setCategories(categories);
}

