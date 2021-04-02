#include "BrassicaInterop_stub.h"
#include "mainwindow.h"

#include <QFileDialog>
#include <QGridLayout>
#include <QLabel>
#include <QMenu>
#include <QMenuBar>
#include <QTextCodec>
#include <QTextStream>
#include <Qt>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
{
    setWindowTitle("Brassica");

    QWidget *window = new QWidget;
    setCentralWidget(window);

    setupWidgets(window);
    setupMenuBar();

    connect(applyBtn, &QPushButton::clicked, this, &MainWindow::applySoundChanges);
    connect(reportRulesBtn, &QPushButton::clicked, this, &MainWindow::reportRulesApplied);
    connect(categoriesEdit, &QPlainTextEdit::textChanged, this, &MainWindow::reparseCategories);
}

MainWindow::~MainWindow()
{
}

void MainWindow::setupWidgets(QWidget *central)
{
    QGridLayout *mainLayout = new QGridLayout(central);

    QSplitter *mainSplitter = new QSplitter(Qt::Horizontal);
    mainLayout->addWidget(mainSplitter);

    QVBoxLayout *categoriesLayout = mkLayoutWithContainer(mainSplitter);
    QVBoxLayout *rulesLayout      = mkLayoutWithContainer(mainSplitter);
    QVBoxLayout *wordsLayout      = mkLayoutWithContainer(mainSplitter);
    QVBoxLayout *midLayout        = mkLayoutWithContainer(mainSplitter);
    QVBoxLayout *outputLayout     = mkLayoutWithContainer(mainSplitter);

    midLayout->setAlignment(Qt::AlignTop);

    const QFont fixedFont = QFontDatabase::systemFont(QFontDatabase::FixedFont);

    QLabel *categoriesLbl = new QLabel("Categories:");
    categoriesEdit = new QPlainTextEdit;
    categoriesEdit->setFont(fixedFont);
    categoriesLayout->addWidget(categoriesLbl);
    categoriesLayout->addWidget(categoriesEdit);

    QLabel *rulesLbl = new QLabel("Rules:");
    rulesEdit = new QPlainTextEdit;
    rulesEdit->setFont(fixedFont);
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

    QLabel *outputLbl = new QLabel("Output lexicon:");
    outputEdit = new QTextEdit;
    outputEdit->setReadOnly(true);
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
}

QVBoxLayout *MainWindow::mkLayoutWithContainer(QSplitter *splitter)
{
    QWidget *container = new QWidget;
    QVBoxLayout *layout = new QVBoxLayout(container);
    layout->setContentsMargins(0,0,0,0);
    splitter->addWidget(container);
    return layout;
}

void MainWindow::applySoundChanges()
{
    QString categories = categoriesEdit->toPlainText();
    QString rules      = rulesEdit     ->toPlainText();
    QString words      = wordsEdit     ->toPlainText();

    //QString output = proc->applyRules(categories, rules, words);

    QByteArray output = QByteArray((char*) parseTokeniseAndApplyRules_hs(categories.toUtf8().data(), rules.toUtf8().data(), words.toUtf8().data(), false));
    outputEdit->setHtml(output);
}

void MainWindow::reportRulesApplied()
{
    QString categories = categoriesEdit->toPlainText();
    QString rules      = rulesEdit     ->toPlainText();
    QString words      = wordsEdit     ->toPlainText();

    //QString output = proc->applyRules(categories, rules, words);

    QByteArray output = QByteArray((char*) parseTokeniseAndApplyRules_hs(categories.toUtf8().data(), rules.toUtf8().data(), words.toUtf8().data(), true));
    outputEdit->setHtml(output);
}

void MainWindow::openRules()
{
    QString fileName = QFileDialog::getOpenFileName(this, "Open rules", QString(), "Brassica rules (*.bsc);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    QTextStream in(&file);
    in.setCodec(QTextCodec::codecForName("UTF-8"));
    QString line;
    while (!in.atEnd() && (line = in.readLine()) != "[rules]")
        categoriesEdit->appendPlainText(line);
    while (in.readLineInto(&line))
        rulesEdit->appendPlainText(line);
}

void MainWindow::saveRules()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Save rules", QString(), "Brassica rules (*.bsc);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString categories = categoriesEdit->toPlainText();
    QString rules      = rulesEdit     ->toPlainText();
    QString words      = wordsEdit     ->toPlainText();

    file.write(categories.toUtf8());
    if (!categories.endsWith('\n')) file.write("\n");
    file.write("[rules]\n");
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

void MainWindow::reparseCategories()
{
    const QString categoriesText = categoriesEdit->toPlainText();

    QStringList categories;

    /* A very simple state machine. When a newline is reached, the next
     * character starts a category name. When an equals sign is reached,
     * everything from that character to the previous character is retrrieved
     * and added to `categories`.
     */

    int curCatStart;
    for (int i=0; i<categoriesText.length(); ++i)
    {
        if (categoriesText[i] == '=')
            categories.append(categoriesText.mid(curCatStart, i-curCatStart).trimmed());
        else if (categoriesText[i] == '\n')
            curCatStart = i+1;
    }

    rulesHl->setCategories(categories);
}

