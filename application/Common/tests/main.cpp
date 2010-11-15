#include <QCoreApplication>
#include <QTest>

#include <Tests.h>

int main(int argc, char *argv[])
{
   Tests tests;
   return QTest::qExec(&tests, argc, argv);
}
