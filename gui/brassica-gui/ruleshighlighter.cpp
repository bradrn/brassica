#include "ruleshighlighter.h"

RulesHighlighter::RulesHighlighter(QTextDocument *parent)
    : QSyntaxHighlighter(parent)
{
    QTextCharFormat specialCharsFormat;
    specialCharsFormat.setForeground(QColor(0, 0, 255));
    formats.append(specialCharsFormat);
    patterns.append(QRegularExpression(R"(>|#|\(|\)|{|}|\\|\^|%|~|\*|categories|end|new|noreplace|feature|auto|extra|filter|report|@[0-9]+|@\?)"));

    QTextCharFormat separatorFormat;
    separatorFormat.setFontWeight(QFont::Bold);
    formats.append(separatorFormat);
    patterns.append(QRegularExpression("/|_|→|->"));

    QTextCharFormat featureFormat;
    featureFormat.setForeground(QColor(34, 139, 34));
    formats.append(featureFormat);
    patterns.append(QRegularExpression(R"(\$[^\s#[\](){}>\\→/_^%~*@$]+(#[^\s#[\](){}>\\→/_^%~*@$]+)?)"));

    QTextCharFormat flagFormat;
    flagFormat.setForeground(QColor(0, 128, 128));
    formats.append(flagFormat);
    patterns.append(QRegularExpression(R"(^-(x|1|ltr|rtl|\?\?|\?))"));

    QTextCharFormat commentFormat;
    commentFormat.setForeground(QColor(0, 128, 0));
    commentFormat.setFontItalic(true);
    formats.append(commentFormat);
    patterns.append(QRegularExpression(R"(;.*)"));

    categoryFormat = QTextCharFormat();
    categoryFormat.setBackground(QColor(245, 245, 220));
    setCategories(QStringList(), true);
}

void RulesHighlighter::setCategories(QStringList categories, bool forceUpdate /*= false*/)
{
    if (forceUpdate || m_categories != categories)
    {
        m_categories = QStringList(categories);

        QString regexpText(R"(\[.*?\])");  // start with regexp for ad-hoc category
        categories.sort();
        for (int i = categories.length()-1; i >= 0; --i)  // add custom categories in reverse order to get longest match
        {
            regexpText += '|' + QRegularExpression::escape(categories[i]);
        }
        categoriesPattern = QRegularExpression(regexpText);

        rehighlight();
    }
}

void RulesHighlighter::highlightBlock(const QString &text)
{
    {
        QRegularExpressionMatchIterator itr = categoriesPattern.globalMatch(text);
        while (itr.hasNext())
        {
            QRegularExpressionMatch m = itr.next();
            setFormat(m.capturedStart(), m.capturedLength(), categoryFormat);
        }
    }

    for (int i = 0; i < patterns.length(); i++)
    {
        QRegularExpressionMatchIterator itr = patterns[i].globalMatch(text);
        while (itr.hasNext())
        {
            QRegularExpressionMatch m = itr.next();
            setFormat(m.capturedStart(), m.capturedLength(), formats[i]);
        }
    }
}
