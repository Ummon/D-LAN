#include <QCoreApplication>

#include <QTcpServer>

#include <Downloader.h>
#include <Listener.h>

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   const int PORT(12345);

   Listener listener(PORT);
   Downloader downloader(&a, PORT);

   return a.exec();
}
