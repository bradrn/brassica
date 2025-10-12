#ifndef RULESHIGHLIGHER_H
#define RULESHIGHLIGHER_H

#include <QRegularExpression>
#include <QSyntaxHighlighter>

class RulesHighlighter : public QSyntaxHighlighter
{
    Q_OBJECT
public:
    RulesHighlighter(QTextDocument *parent);

    void setCategories(QStringList categories, bool forceUpdate = false);
    void setHighlights(QList<int> highlights);
    void setErrors(QList<int> errors);

protected:
    void highlightBlock(const QString &text) override;

private:
    QList<int> highlights;
    QList<int> errors;

    QTextCharFormat categoryFormat;
    QTextCharFormat highlightFormat;
    QTextCharFormat errorFormat;

    // NB. these two shouldn't get out of sync!
    QStringList m_categories;
    QRegularExpression categoriesPattern;

    // and neither should these
    QVector<QTextCharFormat> formats;
    QVector<QRegularExpression> patterns;
};

#endif // RULESHIGHLIGHER_H
