#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "HsFFI.h"
#include "ruleshighlighter.h"

#include <QCheckBox>
#include <QGroupBox>
#include <QMainWindow>
#include <QPlainTextEdit>
#include <QProcess>
#include <QPushButton>
#include <QRadioButton>
#include <QSplitter>
#include <QVBoxLayout>

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private:
    QPlainTextEdit *rulesEdit;
    QPlainTextEdit *wordsEdit;
    QPushButton *applyBtn;
    QRadioButton *nohighlightBtn;
    QRadioButton *diffhighlightBtn;
    QRadioButton *inputhighlightBtn;
    QRadioButton *rawBtn;
    QRadioButton *mdfBtn;
    QGroupBox *outputFormatBox;
    QRadioButton *mdfoutBtn;
    QRadioButton *mdfetymoutBtn;
    QRadioButton *rawoutBtn;
    QPushButton *reportRulesBtn;
    QCheckBox *viewLive;
    QTextEdit *outputEdit;

    RulesHighlighter *rulesHl;

    //BrassicaProcess *proc;

    void setupWidgets(QWidget *centralWidget);
    void setupMenuBar();

    QVBoxLayout *mkLayoutWithContainer(QSplitter *splitter);

    HsStablePtr hsResults;

private slots:
    void applySoundChanges(bool live, bool reportRules);

    void openRules();
    void saveRules();
    void openLexicon();
    void saveLexicon();

    void showParadigmBuilder();

    void reparseCategories();
};
#endif // MAINWINDOW_H
