#include "brassicaprocess.h"

#include <QCoreApplication>
#include <QJsonDocument>
#include <QJsonObject>

BrassicaProcess::BrassicaProcess(QObject *parent)
    : QObject(parent)
{
    QString location = QCoreApplication::applicationDirPath() + "/brassica";
    QStringList args("--server");

    proc = new QProcess;
    connect(proc, &QProcess::errorOccurred, this, [this](QProcess::ProcessError e){_errorState=e;});
    proc->start(location, args);
    valid = proc->waitForStarted();
}

BrassicaProcess::~BrassicaProcess()
{
    proc->kill();
    proc->waitForFinished();
    delete proc;
}

bool BrassicaProcess::startupCorrect()
{
    return valid;
}

QProcess::ProcessError BrassicaProcess::errorState()
{
    return _errorState;
}

QString BrassicaProcess::parseTokeniseAndApplyRules(
    QString rules,
    QString words,
    bool reportRules,
    InputLexiconFormat inFmt,
    HighlightMode hlMode,
    OutputMode outMode,
    QJsonValue *&prev,
    QString sep)
{
    QJsonObject req = QJsonObject();
    req.insert("method", "Rules");
    req.insert("changes", rules);
    req.insert("input", words);
    req.insert("report", reportRules);
    req.insert("inFmt", toJson(inFmt));
    req.insert("hlMode", toJson(hlMode));
    req.insert("outMode", toJson(outMode));
    req.insert("prev", *prev);
    req.insert("sep", sep);

    QJsonObject obj = request(QJsonDocument(req)).object();
    QString method = obj.value("method").toString();
    if (method == "Error") {
        return obj.value("contents").toString();
    } else if (method == "Rules") {
        delete prev;
        prev = new QJsonValue(obj.value("prev"));
        return obj.value("output").toString();
    }
}

QString BrassicaProcess::parseAndBuildParadigm(QString paradigm, QString roots)
{
    QJsonObject req = QJsonObject();
    req.insert("method", "Paradigm");
    req.insert("pText", paradigm);
    req.insert("input", roots);

    QJsonObject obj = request(QJsonDocument(req)).object();
    QString method = obj.value("method").toString();
    if (method == "Error") {
        return obj.value("contents").toString();
    } else if (method == "Paradigm") {
        return obj.value("output").toString();
    }
}

QJsonDocument BrassicaProcess::request(QJsonDocument req)
{
    proc->write(req.toJson(QJsonDocument::Compact));
    QByteArray resp;
    do {
        proc->waitForReadyRead();
        resp.append(proc->readAll());
    } while (!resp.endsWith('\027'));
    resp.chop(1);
    return QJsonDocument::fromJson(resp);
}

QString BrassicaProcess::toJson(InputLexiconFormat val)
{
    switch (val) {
        case Raw: return "Raw";
        case MDF: return "MDF";
    }
}

QString BrassicaProcess::toJson(HighlightMode val)
{
    switch(val) {
        case NoHighlight:        return "NoHighlight";
        case DifferentToLastRun: return "DifferentToLastRun";
        case DifferentToInput:   return "DifferentToInput";
    }
}

QString BrassicaProcess::toJson(OutputMode val)
{
    switch(val) {
        case MDFOutput:            return "MDFOutput";
        case WordsOnlyOutput:      return "WordsOnlyOutput";
        case MDFOutputWithEtymons: return "MDFOutputWithEtymons";
        case WordsWithProtoOutput: return "WordsWithProtoOutput";
    }
}
