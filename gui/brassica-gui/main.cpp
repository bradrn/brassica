#include "brassicaprocess.h"
#include "mainwindow.h"

#include <QApplication>
#include <QProcess>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    QCoreApplication::setOrganizationName("bradrn");
    QCoreApplication::setOrganizationDomain("bradrn.com");
    QCoreApplication::setApplicationName("Brassica");
    QCoreApplication::setApplicationVersion("0.1.0");

    BrassicaProcess proc = BrassicaProcess();
    if (!proc.startupCorrect()) {
        qFatal("main: cannot create Brassica child process! error code %d",
               proc.errorState());
    }

    MainWindow w(&proc);
    w.show();

    return a.exec();
}
