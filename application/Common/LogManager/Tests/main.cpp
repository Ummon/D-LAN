#include <QCoreApplication>
#include <QTest>

#include <Tests.h>

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);
  
   Tests tests;
   QTest::qExec(&tests, argc, argv);    
    
   return a.exec();
}
