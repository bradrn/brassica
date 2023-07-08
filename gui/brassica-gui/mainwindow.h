#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "HsFFI.h"
#include "ruleshighlighter.h"
#include "settings.h"

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
    QScrollBar *wordsEditVScroll;
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
    QRadioButton *inoutBtn;
    QPushButton *reportRulesBtn;
    QCheckBox *viewLive;
    QCheckBox *synchroniseScrolls;
    QTextEdit *outputEdit;
    QScrollBar *outputEditVScroll;

    bool blockScrollTrackingEvent = false;

    RulesHighlighter *rulesHl;

    //BrassicaProcess *proc;

    void setupWidgets(QWidget *centralWidget);
    void setupMenuBar();

    QVBoxLayout *mkLayoutWithContainer(QSplitter *splitter);

    void doSaveRules(QString fileName);
    void doSaveLexicon(QString fileName);

    QString currentRulesFile;
    QString currentLexiconFile;

    Settings settings;
    void applySettings();

    HsStablePtr hsResults;

private slots:
    void applySoundChanges(bool live, bool reportRules);

    void openRules();
    void saveRules();
    void saveRulesAs();
    void openLexicon();
    void saveLexicon();
    void saveLexiconAs();

    void updateOutputFromWordsSlider(int value);
    void updateWordsFromOutputSlider(int value);

    void showParadigmBuilder();

    void reparseCategories();

    void editSettings();
};
#endif // MAINWINDOW_H
