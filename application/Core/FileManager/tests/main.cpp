#include <QCoreApplication>
#include <QTest>

#include <Tests.h>
#include <Common/LogManager/Builder.h>

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   Tests tests;
   QTest::qExec(&tests, argc, argv);

   return 0;
}
