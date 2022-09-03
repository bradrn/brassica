#ifndef SETTINGS_H
#define SETTINGS_H

#include <QFont>

class Settings
{
public:
    Settings();

    void writeSettings();

    QFont rulesFont;
    QFont wordsFont;
};

#endif // SETTINGS_H
