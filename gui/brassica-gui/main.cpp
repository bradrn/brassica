#include "HsFFI.h"
// #include "brassicaprocess.h"
#include "mainwindow.h"

#include <QApplication>
#include <QProcess>

int main(int argc, char *argv[])
{
    hs_init(&argc, &argv);

    QApplication a(argc, argv);

//    BrassicaProcess *proc = new BrassicaProcess();
//    if (!proc->startupCorrect())
//        qFatal("main: cannot create Brassica child process");

    MainWindow w;
    w.show();

    int retval = a.exec();

    hs_exit();

    return retval;
}
