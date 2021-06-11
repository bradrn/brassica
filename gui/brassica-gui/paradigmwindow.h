#ifndef PARADIGMWINDOW_H
#define PARADIGMWINDOW_H

#include <QMainWindow>

class QPlainTextEdit;
class QTextEdit;

class ParadigmWindow : public QMainWindow
{
    Q_OBJECT
public:
    ParadigmWindow(QWidget *parent = nullptr);

private slots:
    void rebuildResult();

private:
    QPlainTextEdit *paradigmEdit;
    QPlainTextEdit *rootsEdit;
    QTextEdit *outputEdit;

};

#endif // PARADIGMWINDOW_H
