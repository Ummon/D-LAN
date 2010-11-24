#include <QtCore/QCoreApplication>
#include <QString>
#include <QTextCodec>
#include <QTextStream>

#include <Common/Version.h>

#include <Core.h>

int main(int argc, char *argv[])
{
   QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));

   CoreSpace::Core core(argc, argv);
   return core.exec();
}
