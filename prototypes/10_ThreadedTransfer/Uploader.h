#ifndef UPLOADER_H
#define UPLOADER_H

#include <QThread>
#include <QTcpSocket>

class Uploader : public QThread
{
   static const int BUFFER_SIZE;
   static const int SOCKET_BUFFER_SIZE;
   static const int SOCKET_TIMEOUT;
   Q_OBJECT
public:
   Uploader(QObject* parent, QTcpSocket* socket);
   ~Uploader();

protected:
   void run();

private:
   QTcpSocket* socket;
   QThread* mainThread;
};

#endif
