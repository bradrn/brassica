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
    void saveParadigmAs();
    void openLexicon();
    void saveLexicon();
    void saveLexiconAs();

    void paradigmModified();
    void lexiconModified();

private:
    QPlainTextEdit *paradigmEdit;
    QPlainTextEdit *rootsEdit;
    QTextEdit *outputEdit;
    QCheckBox *separateLinesBox;

    BrassicaProcess *proc;

    QString currentParadigmFile;
    QString currentLexiconFile;

    bool paradigmDirty = false;
    bool lexiconDirty = false;

    void doSaveParadigm(QString fileName);
    void doSaveLexicon(QString fileName);

    void refreshTitle();
};

#endif // PARADIGMWINDOW_H
