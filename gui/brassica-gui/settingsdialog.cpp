#include "settingsdialog.h"

#include <QDialogButtonBox>
#include <QFontDialog>
#include <QGridLayout>
#include <QLabel>
#include <QPushButton>

SettingsDialog::SettingsDialog(Settings &settings, QWidget *parent)
    : QDialog(parent)
    , settings(settings)
{
    setWindowTitle("Settings");

    QGridLayout *layout = new QGridLayout();

    QLabel *rulesFontLbl = new QLabel("Font for rules");
    layout->addWidget(rulesFontLbl, 0, 0);

    QPushButton *rulesFontBtn = new QPushButton("Choose font");
    layout->addWidget(rulesFontBtn, 0, 1);

    QLabel *wordsFontLbl = new QLabel("Font for words");
    layout->addWidget(wordsFontLbl, 1, 0);

    QPushButton *wordsFontBtn = new QPushButton("Choose font");
    layout->addWidget(wordsFontBtn, 1, 1);

    QDialogButtonBox *btns = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel);
    layout->addWidget(btns, 2, 0, 1, 2);

    setLayout(layout);

    connect(rulesFontBtn, &QPushButton::clicked, this, &SettingsDialog::chooseRulesFont);
    connect(wordsFontBtn, &QPushButton::clicked, this, &SettingsDialog::chooseWordsFont);
    connect(btns, &QDialogButtonBox::accepted, this, &SettingsDialog::accept);
    connect(btns, &QDialogButtonBox::rejected, this, &SettingsDialog::reject);
}

void SettingsDialog::chooseRulesFont()
{
    settings.rulesFont = QFontDialog::getFont(nullptr, settings.rulesFont, this);
}

void SettingsDialog::chooseWordsFont()
{
    settings.wordsFont = QFontDialog::getFont(nullptr, settings.wordsFont, this);
}
