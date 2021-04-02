#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "ruleshighlighter.h"

#include <QMainWindow>
#include <QPlainTextEdit>
#include <QProcess>
#include <QPushButton>
#include <QSplitter>
#include <QVBoxLayout>

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private:
    QPlainTextEdit *categoriesEdit;
    QPlainTextEdit *rulesEdit;
    QPlainTextEdit *wordsEdit;
    QPushButton *applyBtn;
    QPushButton *reportRulesBtn;
    QTextEdit *outputEdit;

    RulesHighlighter *rulesHl;

    //BrassicaProcess *proc;

    void setupWidgets(QWidget *centralWidget);
    void setupMenuBar();

    QVBoxLayout *mkLayoutWithContainer(QSplitter *splitter);

private slots:
    void applySoundChanges();
    void reportRulesApplied();

    void openRules();
    void saveRules();
    void openLexicon();
    void saveLexicon();

    void reparseCategories();
};
#endif // MAINWINDOW_H
