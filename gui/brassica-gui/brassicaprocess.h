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
    ~BrassicaProcess();

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

    QString parseTokeniseAndApplyRules(QString rules,
        QString words,
        ReportMode reportRules,
        InputLexiconFormat inFmt,
        HighlightMode hlMode,
        OutputMode outMode,
        QJsonValue *&prev, QString sep);
    QString parseAndBuildParadigm(QString paradigm, QString roots, bool separateLines);

private:
    QProcess *proc;
    bool valid;
    QProcess::ProcessError _errorState;  // if any

    QJsonDocument request(QJsonDocument req);

    QString toJson(InputLexiconFormat val);
    QJsonValue toJson(ReportMode val);
    QString toJson(HighlightMode val);
    QString toJson(OutputMode val);
};

#endif // BRASSICAPROCESS_H
