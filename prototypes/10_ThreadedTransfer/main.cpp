#include <QCoreApplication>

#include <QTcpServer>

#include <Downloader.h>
#include <Listener.h>

void printUsage(QString progName)
{
   qDebug() << "Usage : " << progName << " [ uploader | downloader [address] ]";
}

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   const int PORT(12345);

   if (argc <= 1)
   {
      Listener listener(PORT);
      Downloader downloader(&a, "localhost", PORT);
      return a.exec();
   }

   QString param(argv[1]);
   if (param == "uploader")
   {
      Listener listener(PORT);
      return a.exec();
   }
   else if (param == "downloader")
   {
      QString address = "localhost";
      if (argc >= 3)
         address = argv[2];
      Downloader downloader(&a, address, PORT);
      return a.exec();
   }

   printUsage(argv[0]);
   return 1;
}
