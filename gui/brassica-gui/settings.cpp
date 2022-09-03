#include "settings.h"

#include <QFontDatabase>
#include <QSettings>

Settings::Settings()
{
    QSettings s;  // use default application & organisation names, from main function

    QVariant it = s.value("rulesFont");
    if (!it.isNull())
        rulesFont = it.value<QFont>();
    else
        rulesFont = QFontDatabase::systemFont(QFontDatabase::FixedFont);

    it = s.value("wordsFont");
    if (!it.isNull())
        wordsFont = it.value<QFont>();
    else
        wordsFont = QFont("Microsoft Sans Serif", 8);  // there seem to be inconsistencies with saving if the font size here isn't specified...
}

void Settings::writeSettings()
{
   QSettings s;
   s.setValue("rulesFont", rulesFont);
   s.setValue("wordsFont", wordsFont);
}
