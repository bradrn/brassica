#include "BrassicaInterop_stub.h"
#include "paradigmwindow.h"

#include <QPlainTextEdit>
#include <QGridLayout>
#include <QLabel>
#include <QPushButton>

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

    connect(buildBtn, &QPushButton::clicked, this, &ParadigmWindow::rebuildResult);

    // previous code for live previewing (turned out to be too slow):
    //connect(paradigmEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::rebuildResult);
    //connect(rootsEdit, &QPlainTextEdit::textChanged, this, &ParadigmWindow::rebuildResult);
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

