#ifndef PARADIGMWINDOW_H
#define PARADIGMWINDOW_H

#include <QMainWindow>

class BrassicaProcess;
class QPlainTextEdit;
class QTextEdit;
class QCheckBox;

class ParadigmWindow : public QMainWindow
{
    Q_OBJECT
public:
    ParadigmWindow(BrassicaProcess *proc, QWidget *parent = nullptr);

private slots:
    void rebuildResult();

    void openParadigm();
    void saveParadigm();
    void openLexicon();
    void saveLexicon();

private:
    QPlainTextEdit *paradigmEdit;
    QPlainTextEdit *rootsEdit;
    QTextEdit *outputEdit;
    QCheckBox *separateLinesBox;

    BrassicaProcess *proc;
};

#endif // PARADIGMWINDOW_H
