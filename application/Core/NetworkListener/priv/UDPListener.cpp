#include <priv/UDPListener.h>

using namespace NetworkListener;

#include <Common/LogManager/Builder.h>

#if defined(Q_OS_LINUX)
   #include <netinet/in.h>
#elif defined(Q_OS_WIN32)
   #include <Winsock.h>
#endif

//Constantes
const char UDPListener::TTL = 3;
const int UDPListener::port = 34326;
QHostAddress UDPListener::multicastIP("236.123.43.24");

/**
 * Constructor of UDPListener, initialize the socket to broadcast
 *
 * @author mcuony
 */
::UDPListener::UDPListener(QSharedPointer<PeerManager::IPeerManager> newPeerManager) : QObject() , logger(LogManager::Builder::newLogger("NetworkListener::UDPListener"))
{

   this->logger->log("Loading ..", LogManager::EndUser);

   this->peerManager = newPeerManager;

   // Creating and setting options to the socket.
   this->socket = new QUdpSocket(this);

   if (!this->socket->bind(UDPListener::port, QUdpSocket::ReuseAddressHint))
      this->logger->log("Can't bind", LogManager::FatalError);

   if (!connect(this->socket, SIGNAL(readyRead()), this, SLOT(processPendingDatagrams())))
      this->logger->log("Can't listen", LogManager::FatalError);

   int socketDescriptor = this->socket->socketDescriptor();

   // 'loop' is activated only for tests.
   char loop = 1;
   if (setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof loop))
      this->logger->log("Can't set socket option : IP_MULTICAST_LOOP", LogManager::FatalError);

   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_TTL, &UDPListener::TTL, sizeof UDPListener::TTL))
      this->logger->log("Can't set socket option : IP_MULTICAST_TTL : " + error, LogManager::FatalError);

   // 'htonl' reverse the order of the bytes, see : http://www.opengroup.org/onlinepubs/007908799/xns/htonl.html
   struct ip_mreq mreq;
   mreq.imr_multiaddr.s_addr = htonl(UDPListener::multicastIP.toIPv4Address());
   mreq.imr_interface.s_addr = htonl(INADDR_ANY);
   #if defined(Q_OS_LINUX)
      if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof mreq))
   #elif defined(Q_OS_WIN32)
      if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*)&mreq, sizeof mreq))
   #endif
         this->logger->log("Can't set socket option : IP_ADD_MEMBERSHIP : " + error, LogManager::FatalError);

   this->logger->log("Done", LogManager::EndUser);
}

/**
 * Function called when data is recevied by the socket : The coresponding proto is created and the coresponding event is rised
 *
 * @author mcuony
 */
void ::UDPListener::processPendingDatagrams()
{

   QTextStream out(stdout);

   while (this->socket->hasPendingDatagrams())
   {
      QByteArray datagram;
      datagram.resize(this->socket->pendingDatagramSize());
      QHostAddress peerAddress;
      this->socket->readDatagram(datagram.data(), datagram.size(), &peerAddress);

      //this->logger->log("Recived from " +  peerAddress.toString() + " message " + datagram.data(), LogManager::Debug);

      switch (datagram.data()[0])
      {
         case chatMessagePacket:
         {
            // We create a new chatMessage.
            Protos::Core::ChatMessage chatMessage;

            // We get the correct string.
            QString data = datagram.data();
            std::string input = data.mid(1).toStdString();

            // We convert in into a proto.
            chatMessage.ParseFromString(input);

            // And we rise the event.
            emit newChatMessage(chatMessage);

            break;
         }

         case IAmAlivePacket:
         {
            // We create a new IMAlimeMessage.
            Protos::Core::HaveChunks IMAlimeMessage;
            IMAlimeMessage.ParseFromString(datagram.mid(1).data());
            quint64 amount = IMAlimeMessage.amount();
            QString nick(IMAlimeMessage.nick().data());
            Common::Hash id(IMAlimeMessage.peerid().hash().data());

            //We forward the information to the PeerManager
            this->peerManager->updatePeer(id, peerAddress.toIPv4Address(), nick, amount);

            //this->logger->log("Someone is alive: " + id + ", " +data.fromStdString(IMAlimeMessage.nick()), LogManager::Debug);

            break;
         }

         case findPacket:
         {
            // We create a new findMessage.
            Protos::Core::Find findMessage;
            findMessage.ParseFromString(datagram.mid(1).data());

            emit newFindRequset(findMessage);

            //ISO C++ says that these are ambiguous, even though the worst conversion for the first is better than the worst conversion for the secondthis->logger->log("Find request id " + QString::fromStdString(findMessage.DebugString())  + QString::number( findMessage.tag() ), LogManager::Debug);

            break;
         }

         case findResultPacket:
         {
            // We create a new findMessage.
            Protos::Common::FindResult findRMessage;
            findRMessage.ParseFromString(datagram.mid(1).data());

            emit newFindResult(findRMessage);

            //this->logger->log("Someone is alive: " + id + ", " +data.fromStdString(IMAlimeMessage.nick()), LogManager::Debug);

            break;
         }




         default:
         {
            this->logger->log("Unknow type ???", LogManager::Debug);
            break;
         }
      }
   }
}

/**
  * Send an Udp multicast message
  *
  * @param mess : The message to send
  * @author mcuony
  */
bool ::UDPListener::sendMessage(const QByteArray& datagram)
{
   //this->logger->log("Sending " + mess, LogManager::Debug);

   //QByteArray datagram = mess.toUtf8();

   if (this->socket->writeDatagram(
      datagram.data(),
      datagram.size(),
      UDPListener::multicastIP,
      UDPListener::port
      ) == -1)
   {
      this->logger->log("Unable to send datagram", LogManager::FatalError);
      return false;
   }
   else
   {
      return true;
   }
}
