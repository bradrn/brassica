#include "finddialog.h"

#include <QDialogButtonBox>
#include <QGroupBox>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QRadioButton>
#include <QVBoxLayout>

FindDialog::FindDialog(FindArea area, QWidget *parent)
    : area(area)
{
    setWindowTitle("Find");

    QVBoxLayout *layout = new QVBoxLayout();

    QHBoxLayout *textLayout = new QHBoxLayout();
    layout->addLayout(textLayout);

    QLabel *textLbl = new QLabel("Text to find:");
    textLayout->addWidget(textLbl);

    QLineEdit *text = new QLineEdit();
    textLayout->addWidget(text);

    QGroupBox *areaBox = new QGroupBox("Area to search");
    QVBoxLayout *areaLayout = new QVBoxLayout(areaBox);
    layout->addWidget(areaBox);

    QRadioButton *changesBtn = new QRadioButton("Sound changes");
    areaLayout->addWidget(changesBtn);
    connect(changesBtn, &QRadioButton::toggled, this,
            [this] (bool checked) { if (checked) this->area = Changes; });

    QRadioButton *inputBtn = new QRadioButton("Input words");
    areaLayout->addWidget(inputBtn);
    connect(inputBtn, &QRadioButton::toggled, this,
            [this] (bool checked) { if (checked) this->area = Input; });

    QRadioButton *outputBtn = new QRadioButton("Output words");
    areaLayout->addWidget(outputBtn);
    connect(outputBtn, &QRadioButton::toggled, this,
            [this] (bool checked) { if (checked) this->area = Output; });

    switch (area) {
    case Changes: changesBtn->setChecked(true); break;
    case Input:   inputBtn->setChecked(true); break;
    case Output:  outputBtn->setChecked(true); break;
    }

    layout->addStretch();

    QDialogButtonBox *btns = new QDialogButtonBox();
    layout->addWidget(btns);

    QPushButton *findNextBtn = new QPushButton("Find next");
    btns->addButton(findNextBtn, QDialogButtonBox::ActionRole);
    connect(findNextBtn, &QPushButton::clicked, this,
            [this, text] { emit findNext(text->text(), this->area, { }); });

    QPushButton *findPrevBtn = new QPushButton("Find previous");
    btns->addButton(findPrevBtn, QDialogButtonBox::ActionRole);
    connect(findPrevBtn, &QPushButton::clicked, this,
            [this, text] { emit findNext(text->text(), this->area, QTextDocument::FindBackward); });

    setLayout(layout);
}
