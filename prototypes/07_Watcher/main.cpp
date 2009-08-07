#include <QtCore/QCoreApplication>

#include <DirWatcher.h>

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   DirWatcher* dirWatcher = DirWatcher::getNewWatcher();
   dirWatcher->addDir("dirs/a");
   dirWatcher->addDir("dirs/b");

   forever
   {
      dirWatcher->waitEvent(100000); // 100 seconds
   }
   
   delete dirWatcher;

   return a.exec();
}
