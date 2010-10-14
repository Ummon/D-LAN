#include <priv/TCPListener.h>
using namespace NL;

#include <Common/Settings.h>

#include <priv/Log.h>

/**
  * @class TCPListener
  * @author mcuony
  * @author gburri
  */

const int TCPListener::MAX_LISTEN_ATTEMPT(10);

TCPListener::TCPListener(QSharedPointer<PM::IPeerManager> peerManager)
   : peerManager(peerManager)
{   
   this->currentPort = SETTINGS.get<quint32>("unicast_base_port");

   int n = 0;
   while(!this->tcpServer.listen(QHostAddress::Any, this->currentPort))
   {
      if (++n == MAX_LISTEN_ATTEMPT)
      {
         L_ERRO("Can't find a port to listen");
         break;
      }
      this->currentPort++;
   }
   connect(&this->tcpServer, SIGNAL(newConnection()), this, SLOT(newConnection()));
}

quint16 TCPListener::getCurrentPort()
{
   return this->currentPort;
}

void TCPListener::newConnection()
{
   QTcpSocket* socket = this->tcpServer.nextPendingConnection();
   this->peerManager->newConnection(socket);
}
