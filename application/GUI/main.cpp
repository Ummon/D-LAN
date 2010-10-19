#include <QtGui/QApplication>
#include <MainWindow.h>
using namespace GUI;

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MainWindow w;
    w.show();

    return a.exec();
}
