#include <Listener.h>

Listener::Listener(const int port)
{
   this->tcpServer.listen(QHostAddress::Any, port);
   connect(&this->tcpServer, SIGNAL(newConnection()), this, SLOT(newConnection()));
}

void Listener::newConnection()
{
   this->uploaders << new Uploader(this, this->tcpServer.nextPendingConnection());
}
