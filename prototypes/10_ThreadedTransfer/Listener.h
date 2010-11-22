#ifndef LISTENER_H
#define LISTENER_H

#include <QObject>
#include <QTcpServer>
#include <QList>

#include <Uploader.h>

class Listener : public QObject
{
   Q_OBJECT
public:
   Listener(const int port);

private slots:
   void newConnection();

private:
   QTcpServer tcpServer;

   QList<Uploader*> uploaders;
};

#endif
