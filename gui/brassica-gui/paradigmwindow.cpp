#include "BrassicaInterop_stub.h"
#include "paradigmwindow.h"

#include <QPlainTextEdit>
#include <QGridLayout>
#include <QLabel>

ParadigmWindow::ParadigmWindow(QWidget *parent)
    : QMainWindow(parent)
{
    setWindowTitle("Brassica Paradigm Builder");

    QWidget *window = new QWidget;
    setCentralWidget(window);

    QGridLayout *layout = new QGridLayout(window);

    QLabel *paradigmLbl = new QLabel("Paradigm:");
    layout->addWidget(paradigmLbl, 0, 0);

    paradigmEdit = new QPlainTextEdit();
    layout->addWidget(paradigmEdit, 1, 0);

    QLabel *rulesLbl = new QLabel("Rules:");
    layout->addWidget(rulesLbl, 0, 1);

    rootsEdit = new QPlainTextEdit();
    layout->addWidget(rootsEdit, 1, 1);

    QLabel *outputLbl = new QLabel("Output:");
    layout->addWidget(outputLbl, 0, 2);

    outputEdit = new QTextEdit();
    outputEdit->setReadOnly(true);
    layout->addWidget(outputEdit, 1, 2);

    connect(paradigmEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::rebuildResult);
    connect(rootsEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::rebuildResult);
}

void ParadigmWindow::rebuildResult()
{
    QString paradigm = paradigmEdit->toPlainText();
    QString roots = rootsEdit->toPlainText();

    QByteArray output =
            QByteArray((char*) parseAndBuildParadigm_hs(
                           paradigm.toUtf8().data(),
                           roots   .toUtf8().data()));

    outputEdit->setHtml(QString::fromUtf8(output));
}

