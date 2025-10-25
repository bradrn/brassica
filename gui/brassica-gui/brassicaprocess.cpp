#include "brassicaprocess.h"

#include <QCoreApplication>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <qjsonvalue.h>

BrassicaProcess::BrassicaProcess(QObject *parent)
    : QObject(parent)
{
    QString location = QCoreApplication::applicationDirPath() + "/brassica";
    QStringList args("--server");

    proc = new QProcess(this);
    connect(proc, &QProcess::errorOccurred, this, [this](QProcess::ProcessError e){_errorState=e;});
    connect(proc, &QProcess::readyReadStandardOutput, this, &BrassicaProcess::procReadyRead);
    proc->start(location, args);
    valid = proc->waitForStarted();
}

bool BrassicaProcess::startupCorrect()
{
    return valid;
}

QProcess::ProcessError BrassicaProcess::errorState()
{
    return _errorState;
}

void BrassicaProcess::parseTokeniseAndApplyRules(
    QString rules,
    QString words,
    ReportMode reportRules,
    InputLexiconFormat inFmt,
    HighlightMode hlMode,
    OutputMode outMode,
    QString sep)
{
    QJsonObject req = QJsonObject();
    req.insert("method", "Rules");
    req.insert("changes", rules);
    req.insert("input", words);
    req.insert("report", toJson(reportRules));
    req.insert("inFmt", toJson(inFmt));
    req.insert("hlMode", toJson(hlMode));
    req.insert("outMode", toJson(outMode));
    req.insert("prev", prev);
    req.insert("sep", sep);

    request(QJsonDocument(req));
}

void BrassicaProcess::parseAndBuildParadigm(QString paradigm, QString roots, bool separateLines)
{
    QJsonObject req = QJsonObject();
    req.insert("method", "Paradigm");
    req.insert("pText", paradigm);
    req.insert("input", roots);
    req.insert("separateLines", separateLines);

    request(QJsonDocument(req));
}

void BrassicaProcess::request(QJsonDocument req)
{
    proc->write(req.toJson(QJsonDocument::Compact));
}

void BrassicaProcess::procReadyRead()
{
    currentResponse.append(proc->readAll());
    if (currentResponse.endsWith('\027')) {
        currentResponse.chop(1);
        QJsonObject obj = QJsonDocument::fromJson(currentResponse).object();
        currentResponse = QByteArray();

        QString method = obj.value("method").toString();
        if (method == "Error") {
            QJsonArray errorsArr = obj.value("highlights").toArray();
            auto errors = QList<int>();
            for (auto i = errorsArr.cbegin(), end = errorsArr.cend(); i != end; ++i) {
                errors.append((*i).toInt());
            }
            emit errorResult(obj.value("message").toString(), errors);
        } else if (method == "Rules") {
            prev = QJsonValue(obj.value("prev"));
            emit rulesResult(obj.value("output").toString());
        } else if (method == "NotApplied") {
            QJsonArray highlightsArr = obj.value("highlights").toArray();
            auto highlights = QList<int>();
            for (auto i = highlightsArr.cbegin(), end = highlightsArr.cend(); i != end; ++i) {
                highlights.append((*i).toInt());
            }
            emit notAppliedResult(highlights);
        } else if (method == "Paradigm") {
            emit paradigmResult(obj.value("output").toString());
        } else {
            emit errorResult(
                "internal error: BrassicaProcess::procReadyRead",
                QList<int>());
        }
    }
}

QString BrassicaProcess::toJson(InputLexiconFormat val)
{
    switch (val) {
        case Raw: return "Raw";
        case MDFStandard: return "MDFStandard";
        case MDFAlternate: return "MDFAlternate";
    }
    return "internal error: BrassicaProcess::toJson(InputLexiconFormat)";
}

QJsonValue BrassicaProcess::toJson(ReportMode val)
{
    switch (val) {
        case NoReport: return QJsonValue();
        case ReportApplied: return "ReportApplied";
        case ReportNotApplied: return "ReportNotApplied";
    }
    return "internal error: BrassicaProcess::toJson(ReportMode)";
}

QString BrassicaProcess::toJson(HighlightMode val)
{
    switch(val) {
        case NoHighlight:        return "NoHighlight";
        case DifferentToLastRun: return "DifferentToLastRun";
        case DifferentToInputAllChanged:   return "DifferentToInputAllChanged";
        case DifferentToInputSpecificRule: return "DifferentToInputSpecificRule";
    }
        return "internal error: BrassicaProcess::toJson(HighlightMode)";
}

QString BrassicaProcess::toJson(OutputMode val)
{
    switch(val) {
        case MDFOutput:            return "MDFOutput";
        case WordsOnlyOutput:      return "WordsOnlyOutput";
        case MDFOutputWithEtymons: return "MDFOutputWithEtymons";
        case WordsWithProtoOutput: return "WordsWithProtoOutput";
        case WordsWithProtoOutputPreserve: return "WordsWithProtoOutputPreserve";
    }
        return "internal error: BrassicaProcess::toJson(OutputMode)";
}
