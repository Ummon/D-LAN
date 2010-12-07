#include <QCoreApplication>
#include <QTest>

#include <Tests.h>
#include <StressTests.h>

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   bool stressMode = false;
   foreach (QString arg, a.arguments())
      if (arg == "-stress")
      {
         stressMode = true;
         break;
      }

   if (stressMode)
   {
      StressTests tests;
      return QTest::qExec(&tests, argc, argv);
   }
   else
   {
      Tests tests;
      return QTest::qExec(&tests, argc, argv);
   }
}
