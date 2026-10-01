#ifndef BRASSICAPROCESS_H
#define BRASSICAPROCESS_H

#include <QJsonDocument>
#include <QProcess>
#include <qjsonvalue.h>

class BrassicaProcess : public QObject
{
    Q_OBJECT

public:
    BrassicaProcess(QObject *parent = nullptr);

    bool startupCorrect();
    QProcess::ProcessError errorState();

    enum InputLexiconFormat
    {
        Raw,
        MDFStandard,
        MDFAlternate
    };
    enum ReportMode {
        NoReport,
        ReportApplied,
        ReportNotApplied
    };
    enum HighlightMode {
      NoHighlight,
      DifferentToLastRun,
      DifferentToInputAllChanged,
      DifferentToInputSpecificRule
    };
    enum OutputMode
    {
        MDFOutput,
        WordsOnlyOutput,
        MDFOutputWithEtymons,
        WordsWithProtoOutput,
        WordsWithProtoOutputPreserve
    };

    void parseTokeniseAndApplyRules(QString rules,
        QString words,
        ReportMode reportRules,
        InputLexiconFormat inFmt,
        HighlightMode hlMode,
        OutputMode outMode,
        QString sep,
        int timeout /* microseconds */);
    void parseAndBuildParadigm(QString paradigm, QString roots, bool separateLines, int timeout);

private:
    QProcess *proc;
    bool valid;
    QProcess::ProcessError _errorState;  // if any

    void request(QJsonDocument req);

    QString toJson(InputLexiconFormat val);
    QJsonValue toJson(ReportMode val);
    QString toJson(HighlightMode val);
    QString toJson(OutputMode val);

    QByteArray currentResponse;

    QJsonValue prev;

private slots:
    void procReadyRead();

signals:
    void rulesResult(QString output);
    void paradigmResult(QString output);
    void notAppliedResult(QList<int> highlights);
    void errorResult(QString output, QList<int> errors);
};

#endif // BRASSICAPROCESS_H
