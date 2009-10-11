#include <QtCore/QCoreApplication>
#include <QString>
#include <QTextCodec>
#include <QTextStream>

#include "Core.h"

int main(int argc, char *argv[])
{
   QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));
   QCoreApplication a(argc, argv);
   Core::Core core;


   return a.exec();
}
