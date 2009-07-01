#include "Chat.h"

#include <QtCore/QDebug>

#if defined(Q_OS_LINUX)
   #include <netinet/in.h>
   #include <arpa/inet.h>
#elif (Q_OS_WIN32)
   #include <winsock2.h>
#endif

Chat::Chat()
{
   this->socket = new QUdpSocket(this);
     
   if (!this->socket->bind(Chat::port, QUdpSocket::ReuseAddressHint))
      qDebug() << "Can't bind";
   
   if (!connect(this->socket, SIGNAL(readyRead()), this, SLOT(processPendingDatagrams())))
      qDebug() << "Can't listen";
   
   int socketDescriptor = this->socket->socketDescriptor();
   
   // 'loop' is activated only for tests.
   u_char loop = 1;
   if (setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof loop))
      qDebug() << "Can't set socket option : IP_MULTICAST_LOOP";
   
   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_TTL, &Chat::TTL, sizeof Chat::TTL))
      qDebug() << "Can't set socket option : IP_MULTICAST_TTL : " << error;

   // Warning! : 
   //  'QHostAddress("236.123.43.24").toIPv4Address()' will not work here
   //  because the IP has to be in little endian coding.
   struct ip_mreq mreq;
   mreq.imr_multiaddr.s_addr = inet_addr(Chat::multicastIP.toAscii().data());
   mreq.imr_interface.s_addr = htonl(INADDR_ANY);
   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof mreq))
      qDebug() << "Can't set socket option : IP_ADD_MEMBERSHIP : " << error;
}

Chat::~Chat()
{
   delete this->socket;
}

void Chat::sendMessage(const QString& mess)
{
   QByteArray datagram = mess.toUtf8();
   if (this->socket->writeDatagram(
      datagram.data(),
      datagram.size(),
      QHostAddress(Chat::multicastIP),
      Chat::port
   ) == -1)
      qDebug() << "Unable to send datagram";      
}

void Chat::processPendingDatagrams()
{
   QTextStream out(stdout);
   while (this->socket->hasPendingDatagrams())
   {
      QByteArray datagram;
      datagram.resize(this->socket->pendingDatagramSize());
      this->socket->readDatagram(datagram.data(), datagram.size());
      out << "Message received : " << datagram.data() << endl;
   }
}

const u_char Chat::TTL = 3;
const int Chat::port = 34326;
QString Chat::multicastIP("236.123.43.24");
