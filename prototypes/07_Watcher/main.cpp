#include <QtCore/QCoreApplication>
#include <QtCore/QDebug>

#include <DirWatcher.h>

int main(int argc, char *argv[])
{  
   QCoreApplication a(argc, argv);

   DirWatcher* dirWatcher = DirWatcher::getNewWatcher();
   try
   {
      dirWatcher->addDir("dirs/a");
      dirWatcher->addDir("dirs/b");
   
      forever
      {
         QList<WatcherEvent> events = dirWatcher->waitEvent();         
      }
   }
   catch (const DirWatcherException& e)
   {
      qDebug() << "Error : " << e.mess;
   }
   
   delete dirWatcher;

   return a.exec();
}
