#ifndef SETTINGSDIALOG_H
#define SETTINGSDIALOG_H

#include "settings.h"

#include <QDialog>

class SettingsDialog : public QDialog
{
    Q_OBJECT
public:
    SettingsDialog(Settings &settings, QWidget *parent = nullptr);

    Settings &settings;
    const Settings oldSettings;

private slots:
    void chooseRulesFont();
    void chooseWordsFont();

    void reject() override;
};

#endif // SETTINGSDIALOG_H
