#include <Uploader.h>

#include <QElapsedTimer>
#include <QDebug>

const int Uploader::BUFFER_SIZE(16 * 1024);
const int Uploader::SOCKET_BUFFER_SIZE(10 * 1024 * 1024);
const int Uploader::SOCKET_TIMEOUT(5000);

Uploader::Uploader(QObject* parent, QTcpSocket* socket)
   : QThread(parent), socket(socket)
{
   qDebug() << "New uploader";

   this->mainThread = QThread::currentThread();
   this->socket->setParent(0);
   this->socket->moveToThread(this);
   this->start();
}

Uploader::~Uploader()
{
   delete this->socket;
}

void Uploader::run()
{
   if (!this->socket || !this->socket->isValid())
   {
      qDebug() << "Unable to upload, invalid socket";
      return;
   }

   qDebug() << "Uploader starting to upload..";

   char buffer[BUFFER_SIZE];

   qint64 totalBytesSent = 0;
   QElapsedTimer timer;
   timer.start();

   forever
   {
      totalBytesSent += this->socket->write(buffer, BUFFER_SIZE);

      if (timer.elapsed() > 1000)
      {
         qDebug() << "Uploader : Total bytes sent : " << totalBytesSent;
         timer.restart();
      }

      if (socket->bytesToWrite() > SOCKET_BUFFER_SIZE)
      {
         if (!socket->waitForBytesWritten(SOCKET_TIMEOUT))
         {
            qDebug() << "Uploader : Cannot write data, timeout, error :" << socket->errorString();
            break;
         }
      }
   }

   this->socket->moveToThread(this->mainThread);
}
