#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "brassicaprocess.h"
#include "finddialog.h"
#include "ruleshighlighter.h"
#include "settings.h"

#include <QCheckBox>
#include <QGroupBox>
#include <QLineEdit>
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

private:
    QPlainTextEdit *rulesEdit;
    QPlainTextEdit *wordsEdit;
    QScrollBar *wordsEditVScroll;
    QPushButton *applyBtn;
    QRadioButton *nohighlightBtn;
    QRadioButton *diffhighlightBtn;
    QRadioButton *inputhighlightBtn;
    QRadioButton *specifichighlightBtn;
    QRadioButton *rawBtn;
    QRadioButton *mdfBtn;
    QRadioButton *mdfAltBtn;
    QGroupBox *outputFormatBox;
    QRadioButton *mdfoutBtn;
    QRadioButton *mdfetymoutBtn;
    QRadioButton *rawoutBtn;
    QRadioButton *inoutBtn;
    QRadioButton *inoutBtnPreserve;
    QPushButton *reportRulesBtn;
    QCheckBox *viewLive;
    QCheckBox *synchroniseScrolls;
    QCheckBox *reportRulesNotApplied;
    QLineEdit *multiResultSep;
    QTextEdit *outputEdit;
    QScrollBar *outputEditVScroll;

    bool blockScrollTrackingEvent = false;
    bool blockLiveUpdate = false;

    RulesHighlighter *rulesHl;

    BrassicaProcess *proc;

    void setupWidgets(QWidget *centralWidget);
    void setupMenuBar();

    QVBoxLayout *mkLayoutWithContainer(QSplitter *splitter);

    void doSaveRules(QString fileName);
    void doSaveLexicon(QString fileName);

    bool checkRulesDirty();
    bool checkLexiconDirty();

    QString currentRulesFile;
    QString currentLexiconFile;

    bool rulesDirty = false;
    bool lexiconDirty = false;

    void refreshTitle();

    FindDialog *findDialog = nullptr;

    Settings settings;
    void applySettings();

    void closeEvent(QCloseEvent *event) override;

    void repopulateExtraSelections();

    QList<int> m_highlights;
    QList<int> m_errors;
    bool m_live;

    QTextCharFormat highlightFormat;
    QTextCharFormat errorFormat;

private slots:
    void applySoundChanges(bool live, BrassicaProcess::ReportMode reportRules);

    void rulesResult(QString output);
    void setHighlights(QList<int> highlights);
    void errorResult(QString output, QList<int> errors);

    void openRules();
    void saveRules();
    void saveRulesAs();
    void openLexicon();
    void saveLexicon();
    void saveLexiconAs();

    void toggleCursor();

    void rulesModified();
    void lexiconModified();

    void updateOutputFromWordsSlider(int value);
    void updateWordsFromOutputSlider(int value);

    void highlightUnusedRules(bool enable);

    void reselectCheckboxes();

    void showFindWindow();
    void findNext(QString substring, FindDialog::FindArea area, QTextDocument::FindFlags flags);

    void showParadigmBuilder();

    void reparseCategories();

    void editSettings();
};
#endif // MAINWINDOW_H
