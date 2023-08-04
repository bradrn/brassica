#include "paradigmwindow.h"

#include <QPlainTextEdit>
#include <QGridLayout>
#include <QLabel>
#include <QPushButton>
#include <QMenu>
#include <QMenuBar>
#include <QFileDialog>

ParadigmWindow::ParadigmWindow(QWidget *parent)
    : QMainWindow(parent)
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

    QPushButton *buildBtn = new QPushButton("Build");
    outputLayout->addWidget(buildBtn);

    QMenu *fileMenu = menuBar()->addMenu("&File");
    fileMenu->addAction("Open paradigm", this, &ParadigmWindow::openParadigm, QKeySequence::Open);
    fileMenu->addAction("Save paradigm", this, &ParadigmWindow::saveParadigm, QKeySequence::Save);
    fileMenu->addAction("Open lexicon", this, &ParadigmWindow::openLexicon);
    fileMenu->addAction("Save lexicon", this, &ParadigmWindow::saveLexicon);

    connect(buildBtn, &QPushButton::clicked, this, &ParadigmWindow::rebuildResult);

    // previous code for live previewing (turned out to be too slow):
    //connect(paradigmEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::rebuildResult);
    //connect(rootsEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::rebuildResult);
}

void ParadigmWindow::rebuildResult()
{
    /*TODO
    QString paradigm = paradigmEdit->toPlainText();
    QString roots = rootsEdit->toPlainText();

    QByteArray output =
            QByteArray((char*) parseAndBuildParadigm_hs(
                           paradigm.toUtf8().data(),
                           roots   .toUtf8().data()));

    outputEdit->setHtml(QString::fromUtf8(output));
    */
}

void ParadigmWindow::openParadigm()
{
    QString fileName = QFileDialog::getOpenFileName(this, "Open paradigm", QString(), "Brassica paradigms (*.bpd);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    paradigmEdit->setPlainText(QString::fromUtf8(file.readAll()));
}

void ParadigmWindow::saveParadigm()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Save paradigm", QString(), "Brassica paradigms (*.bpd);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString rules = paradigmEdit->toPlainText();

    file.write(rules.toUtf8());
}

void ParadigmWindow::openLexicon()
{
    QString fileName = QFileDialog::getOpenFileName(this, "Open lexicon", QString(), "Lexicon files (*.lex);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        return;

    rootsEdit->setPlainText(QString::fromUtf8(file.readAll()));
}

void ParadigmWindow::saveLexicon()
{
    QString fileName = QFileDialog::getSaveFileName(this, "Open lexicon", QString(), "Lexicon files (*.lex);;All files (*.*)");
    QFile file(fileName);
    if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        return;

    QString lexicon = rootsEdit->toPlainText();
    file.write(lexicon.toUtf8());
}
