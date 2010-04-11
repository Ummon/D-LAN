#include "Chat.h"

#include <QtCore/QDebug>

#if defined(Q_OS_LINUX)
   #include <netinet/in.h>
#elif defined(Q_OS_WIN32)
   #include <Winsock.h>
#endif

/**
  * @class Chat
  * An instance of this class will listen for UDP datagram on a certain port and IP (class D).
  * see : http://tldp.org/HOWTO/Multicast-HOWTO-2.html#ss2.1
  * When a message is received it will print it.
  * Multicast nessages can be sended with 'sendMessage'.
  */

Chat::Chat()
{
   this->socket = new QUdpSocket(this);
     
   if (!this->socket->bind(Chat::port, QUdpSocket::ReuseAddressHint))
      qDebug() << "Can't bind";
   
   if (!connect(this->socket, SIGNAL(readyRead()), this, SLOT(processPendingDatagrams())))
      qDebug() << "Can't listen";
   
   int socketDescriptor = this->socket->socketDescriptor();
   
   // 'loop' is activated only for tests.
   char loop = 1;
   if (setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof loop))
      qDebug() << "Can't set socket option : IP_MULTICAST_LOOP";
   
   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_TTL, &Chat::TTL, sizeof Chat::TTL))
      qDebug() << "Can't set socket option : IP_MULTICAST_TTL : " << error;
   
   // 'htonl' reverse the order of the bytes, see : http://www.opengroup.org/onlinepubs/007908799/xns/htonl.html
   struct ip_mreq mreq;
   mreq.imr_multiaddr.s_addr = htonl(Chat::multicastIP.toIPv4Address());
   mreq.imr_interface.s_addr = htonl(INADDR_ANY);
#if defined(Q_OS_LINUX)
   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof mreq))
#elif defined(Q_OS_WIN32)
   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*)&mreq, sizeof mreq))
#endif
      qDebug() << "Can't set socket option : IP_ADD_MEMBERSHIP : " << error;
}

Chat::~Chat()
{
   delete this->socket;
}

/**
  * Send a broadcast message.
  */
void Chat::sendMessage(const QString& mess)
{
   QByteArray datagram = mess.toUtf8();
   if (this->socket->writeDatagram(
      datagram.data(),
      datagram.size(),
      Chat::multicastIP,
      Chat::port
   ) == -1)
      qDebug() << "Unable to send datagram";      
}

/**
  * When a datagram is received this slot is called.
  */
void Chat::processPendingDatagrams()
{
   QTextStream out(stdout);
   while (this->socket->hasPendingDatagrams())
   {
      QByteArray datagram;
      datagram.resize(this->socket->pendingDatagramSize());
      QHostAddress peerAddress;
      this->socket->readDatagram(datagram.data(), datagram.size(), &peerAddress);
      out << "Message received : " << datagram.data() << endl;
      out << "  IP = " << peerAddress.toString() << endl;
   }
}

const char Chat::TTL = 3;
const int Chat::port = 34326;
QHostAddress Chat::multicastIP("236.123.43.24");
