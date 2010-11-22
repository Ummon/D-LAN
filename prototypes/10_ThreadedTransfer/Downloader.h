#ifndef DOWNLOADER_H
#define DOWNLOADER_H

#include <QThread>
#include <QTcpSocket>

class Downloader : public QThread
{
   static const int BUFFER_SIZE;
   static const int SOCKET_TIMEOUT;
   Q_OBJECT
public:
   explicit Downloader(QObject *parent, const int port);

protected:
   void run();

private slots:
   void connected();

private:
   QTcpSocket socket;
   QThread* mainThread;
};

#endif
