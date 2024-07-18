#include "brassicaprocess.h"
#include "paradigmwindow.h"

#include <QPlainTextEdit>
#include <QGridLayout>
#include <QLabel>
#include <QPushButton>
#include <QMenu>
#include <QMenuBar>
#include <QFileDialog>
#include <QCheckBox>

ParadigmWindow::ParadigmWindow(BrassicaProcess *proc, QWidget *parent)
    : QMainWindow(parent)
    , proc(proc)
{
    setWindowTitle("Brassica Paradigm Builder");

    QWidget *window = new QWidget;
    setCentralWidget(window);

    QHBoxLayout *layout = new QHBoxLayout(window);

    QVBoxLayout *paradigmLayout = new QVBoxLayout();
    QVBoxLayout *rootsLayout = new QVBoxLayout();
    QVBoxLayout *outputLayout = new QVBoxLayout();

    layout->addLayout(paradigmLayout);
    layout->addLayout(rootsLayout);
    layout->addLayout(outputLayout);

    QLabel *paradigmLbl = new QLabel("Paradigm:");
    paradigmLayout->addWidget(paradigmLbl);

    paradigmEdit = new QPlainTextEdit();
    paradigmLayout->addWidget(paradigmEdit);

    QLabel *rootsLbl = new QLabel("Roots:");
    rootsLayout->addWidget(rootsLbl);

    rootsEdit = new QPlainTextEdit();
    rootsLayout->addWidget(rootsEdit);

    QLabel *outputLbl = new QLabel("Output:");
    outputLayout->addWidget(outputLbl);

    outputEdit = new QTextEdit();
    outputEdit->setReadOnly(true);
    outputLayout->addWidget(outputEdit);

    separateLinesBox = new QCheckBox("Each word on its own line");
    outputLayout->addWidget(separateLinesBox);

    QPushButton *buildBtn = new QPushButton("Build");
    outputLayout->addWidget(buildBtn);

    QMenu *fileMenu = menuBar()->addMenu("&File");
    fileMenu->addAction("Open paradigm", QKeySequence(Qt::CTRL | Qt::Key_O), this, &ParadigmWindow::openParadigm);
    fileMenu->addAction("Save paradigm", QKeySequence(Qt::CTRL | Qt::Key_S), this, &ParadigmWindow::saveParadigm);
    fileMenu->addAction("Save paradigm as", this, &ParadigmWindow::saveParadigmAs);
    fileMenu->addAction("Open lexicon", QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_O), this, &ParadigmWindow::openLexicon);
    fileMenu->addAction("Save lexicon", QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_S), this, &ParadigmWindow::saveLexicon);
    fileMenu->addAction("Save lexicon as", this, &ParadigmWindow::saveLexiconAs);

    connect(buildBtn, &QPushButton::clicked, this, &ParadigmWindow::rebuildResult);

    connect(paradigmEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::paradigmModified);
    connect(rootsEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::lexiconModified);

    // previous code for live previewing (turned out to be too slow):
    //connect(paradigmEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::rebuildResult);
    //connect(rootsEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::rebuildResult);
}

void ParadigmWindow::rebuildResult()
{
    QString paradigm = paradigmEdit->toPlainText();
    QString roots = rootsEdit->toPlainText();

    QString output = proc->parseAndBuildParadigm(
        paradigm, roots, separateLinesBox->isChecked());

    outputEdit->setHtml(output);
}

void ParadigmWindow::openParadigm()
{
    QString fileName = QFileDialog::getOpenFileName(this, "Open paradigm", QString(), "Brassica paradigms (*.bpd);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    paradigmEdit->setPlainText(QString::fromUtf8(file.readAll()));

    currentParadigmFile = fileName;
    paradigmDirty = false;
    refreshTitle();
}

void ParadigmWindow::saveParadigm()
{
    if (currentParadigmFile.isEmpty()) saveParadigmAs();
    else doSaveParadigm(currentParadigmFile);
}

void ParadigmWindow::saveParadigmAs()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Save paradigm", QString(), "Brassica paradigms (*.bpd);;All files (*.*)");
    doSaveParadigm(fileName);
}

void ParadigmWindow::openLexicon()
{
    QString fileName = QFileDialog::getOpenFileName(this, "Open lexicon", QString(), "Lexicon files (*.lex);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    rootsEdit->setPlainText(QString::fromUtf8(file.readAll()));

    currentLexiconFile = fileName;
    lexiconDirty = false;
    refreshTitle();
}

void ParadigmWindow::saveLexicon()
{
    if (currentLexiconFile.isEmpty()) saveLexiconAs();
    else doSaveLexicon(currentLexiconFile);
}

void ParadigmWindow::saveLexiconAs()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Open lexicon", QString(), "Lexicon files (*.lex);;All files (*.*)");
    doSaveLexicon(fileName);
}

void ParadigmWindow::paradigmModified()
{
    paradigmDirty = paradigmEdit->document()->isModified();
    refreshTitle();
}

void ParadigmWindow::lexiconModified()
{
    lexiconDirty = rootsEdit->document()->isModified();
    refreshTitle();
}

void ParadigmWindow::doSaveParadigm(QString fileName)
{
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString rules = paradigmEdit->toPlainText();

    file.write(rules.toUtf8());

    currentParadigmFile = fileName;
    paradigmDirty = false;
    paradigmEdit->document()->setModified(false);
    refreshTitle();
}

void ParadigmWindow::doSaveLexicon(QString fileName)
{
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString lexicon = rootsEdit->toPlainText();
    file.write(lexicon.toUtf8());

    currentLexiconFile = fileName;
    lexiconDirty = false;
    rootsEdit->document()->setModified(false);
    refreshTitle();
}

void ParadigmWindow::refreshTitle()
{
    // can't use setWindowModified here because we have
    // two separate modified states, so can't use Qt's [*] placeholder

    QString openFiles;

    if (!currentParadigmFile.isEmpty()) {
        QFileInfo paradigmInfo(currentParadigmFile);
        openFiles = paradigmInfo.fileName();
        if (paradigmDirty) openFiles += '*';
    }

    if (!currentLexiconFile.isEmpty()) {
        QFileInfo lexiconInfo(currentLexiconFile);
        if (!openFiles.isEmpty()) openFiles += ", ";
        openFiles += lexiconInfo.fileName();
        if (lexiconDirty) openFiles += '*';
    }

    QString windowTitle("Brassica Paradigm Builder");
    if (!openFiles.isEmpty()) {
        windowTitle += " - ";
        windowTitle += openFiles;
    }

    setWindowTitle(windowTitle);
}
