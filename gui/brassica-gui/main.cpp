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
    QCoreApplication::setApplicationVersion("1.0.0");

    MainWindow w;
    w.show();

    return a.exec();
}
