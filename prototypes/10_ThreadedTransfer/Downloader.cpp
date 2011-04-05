#include <Downloader.h>

#include <QHostAddress>
#include <QElapsedTimer>
#include <QDebug>

const int Downloader::BUFFER_SIZE(16 * 1024);
const int Downloader::SOCKET_TIMEOUT(5000);

Downloader::Downloader(QObject *parent, const QString& address, const int port)
   : QThread(parent)
{
   qDebug() << "New downloader";

   connect(&this->socket, SIGNAL(connected()), this, SLOT(connected()));
   this->socket.connectToHost(address, port);
}

void Downloader::run()
{
   char buffer[BUFFER_SIZE];

   int bytesRead = 0;

   qDebug() << "Downloader starting to download..";

   qint64 totalBytesRead = 0;
   QElapsedTimer timer;
   timer.start();

   forever
   {
      this->msleep(100);
      bytesRead = this->socket.read(buffer, BUFFER_SIZE);

      if (bytesRead == 0)
      {
         qDebug() << "waitForReadyRead";
         if (!this->socket.waitForReadyRead(SOCKET_TIMEOUT))
         {
            qDebug() << "Downloader : Connection dropped! error = " << this->socket.errorString();
            break;
         }
         continue;
      }
      else if (bytesRead == -1)
      {
         qDebug() << "Downloader : Cannot read data from the socket";
         break;
      }

      totalBytesRead += bytesRead;

      if (timer.elapsed() > 1000)
      {
         qDebug() << "Downloader : Total bytes read : " << totalBytesRead;
         timer.restart();
      }
   }

   qDebug() << "Download ended";

   this->socket.moveToThread(this->mainThread);
}

void Downloader::connected()
{
   this->mainThread = QThread::currentThread();
   this->socket.moveToThread(this);
   this->start();
}
