#include "settings.h"

#include <QFontDatabase>

Settings::Settings()
{
    // default settings
    rulesFont = QFontDatabase::systemFont(QFontDatabase::FixedFont);
    wordsFont = QFont("Microsoft Sans Serif");
}
