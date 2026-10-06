#include <Server.h>
using namespace DummyCore;

#include <Connection.h>

Server::Server(const State& state) :
   state(state)
{
   connect(&this->serverIPv4, &QTcpServer::newConnection, this, [this] { this->newConnection(this->serverIPv4); });
   connect(&this->serverIPv6, &QTcpServer::newConnection, this, [this] { this->newConnection(this->serverIPv6); });
}

/**
  * The GUI resolves "localhost" and tries its IPv6 address first: both addresses are listened to, like the Core does.
  * One of them is enough, IPv6 may be disabled.
  */
QString Server::listen(quint16 port)
{
   const bool okIPv4 = this->serverIPv4.listen(QHostAddress::LocalHost, port);
   const bool okIPv6 = this->serverIPv6.listen(QHostAddress::LocalHostIPv6, okIPv4 ? this->serverIPv4.serverPort() : port);

   if (!okIPv4 && !okIPv6)
      return QString("Unable to listen on port %1: %2").arg(port).arg(this->serverIPv4.errorString());

   return QString();
}

quint16 Server::getPort() const
{
   return this->serverIPv4.isListening() ? this->serverIPv4.serverPort() : this->serverIPv6.serverPort();
}

void Server::newConnection(QTcpServer& server)
{
   while (QTcpSocket* socket = server.nextPendingConnection())
   {
      // A connection deletes itself once disconnected, the remaining ones are deleted with the server.
      Connection* connection = new Connection(this->state, socket);
      connection->setParent(this);
      connection->startListening();
   }
}
