#ifndef BRASSICAPROCESS_H
#define BRASSICAPROCESS_H

#include <QJsonDocument>
#include <QProcess>

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
        MDF
    };
    enum HighlightMode
    {
        NoHighlight,
        DifferentToLastRun,
        DifferentToInput
    };
    enum OutputMode
    {
        MDFOutput,
        WordsOnlyOutput,
        MDFOutputWithEtymons,
        WordsWithProtoOutput
    };

    QString parseTokeniseAndApplyRules(QString rules,
        QString words,
        bool reportRules,
        InputLexiconFormat inFmt,
        HighlightMode hlMode,
        OutputMode outMode,
        QJsonValue *&prev);
    QString parseAndBuildParadigm(QString paradigm, QString roots);

private:
    QProcess *proc;
    bool valid;
    QProcess::ProcessError _errorState;  // if any

    QJsonDocument request(QJsonDocument req);

    QString toJson(InputLexiconFormat val);
    QString toJson(HighlightMode val);
    QString toJson(OutputMode val);
};

#endif // BRASSICAPROCESS_H
