#ifndef FINDDIALOG_H
#define FINDDIALOG_H

#include <QDialog>
#include <QTextDocument>

class FindDialog : public QDialog
{
    Q_OBJECT
public:
    enum FindArea
    {
        Changes,
        Input,
        Output
    };

    FindDialog(FindArea area, QWidget *parent = nullptr);

signals:
    void findNext(QString substring, FindArea area, QTextDocument::FindFlags flags);

private:
    FindArea area;
};

#endif // FINDDIALOG_H
