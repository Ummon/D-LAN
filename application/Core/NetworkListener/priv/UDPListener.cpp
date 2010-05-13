#include <priv/UDPListener.h>
using namespace NL;

#include <Common/LogManager/Builder.h>

#if defined(Q_OS_LINUX)
   #include <netinet/in.h>
#elif defined(Q_OS_WIN32)
   #include <Winsock.h>
#endif

//Constantes
const char UDPListener::TTL = 3;
const int UDPListener::multicastPort = 34326;
const int UDPListener::unicastPort = 24614;
QHostAddress UDPListener::multicastIP("236.123.43.24");

/**
 * Constructor of UDPListener, initialize the socket to broadcast
 *
 * @author mcuony
 */
UDPListener::UDPListener(QSharedPointer<PM::IPeerManager> newPeerManager) : QObject() , logger(LM::Builder::newLogger("NetworkListener::UDPListener"))
{
   LOG_USER(this->logger, "Loading ..");

   this->peerManager = newPeerManager;

   // Creating and setting options to the socket.
   this->multicastSocket = new QUdpSocket(this);

   if (!this->multicastSocket->bind(UDPListener::multicastPort, QUdpSocket::ReuseAddressHint))
      LOG_FATA(this->logger, "Can't bind");

   if (!connect(this->multicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingMulticastDatagrams())))
      LOG_FATA(this->logger, "Can't listen");

   int socketDescriptor = this->multicastSocket->socketDescriptor();

   // 'loop' is activated only for tests.
   char loop = 1;
   if (setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_LOOP, &loop, sizeof loop))
      LOG_FATA(this->logger, "Can't set socket option : IP_MULTICAST_LOOP");

   if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_MULTICAST_TTL, &UDPListener::TTL, sizeof UDPListener::TTL))
      LOG_FATA(this->logger, "Can't set socket option : IP_MULTICAST_TTL : " + error);

   // 'htonl' reverse the order of the bytes, see : http://www.opengroup.org/onlinepubs/007908799/xns/htonl.html
   struct ip_mreq mreq;
   mreq.imr_multiaddr.s_addr = htonl(UDPListener::multicastIP.toIPv4Address());
   mreq.imr_interface.s_addr = htonl(INADDR_ANY);
   #if defined(Q_OS_LINUX)
      if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof mreq))
   #elif defined(Q_OS_WIN32)
      if (int error = setsockopt(socketDescriptor, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*)&mreq, sizeof mreq))
   #endif
         LOG_FATA(this->logger, "Can't set socket option : IP_ADD_MEMBERSHIP : " + error);


   // Creating and setting options to the socket.
   this->unicastSocket = new QUdpSocket(this);

   if (!this->unicastSocket->bind(UDPListener::unicastPort, QUdpSocket::ReuseAddressHint))
      LOG_FATA(this->logger, "Can't bind");

   if (!connect(this->unicastSocket, SIGNAL(readyRead()), this, SLOT(processPendingUnicastDatagrams())))
      LOG_FATA(this->logger, "Can't listen");

   //int socketDescriptor = this->unicastSocket->socketDescriptor();


   LOG_USER(this->logger, "Done");
}

/**
 * Function called when data is recevied by the socket : The coresponding proto is created and the coresponding event is rised
 *
 * @author mcuony
 */
void UDPListener::processPendingMulticastDatagrams()
{

   QTextStream out(stdout);

   while (this->multicastSocket->hasPendingDatagrams())
   {
      QByteArray datagram;
      datagram.resize(this->multicastSocket->pendingDatagramSize());
      QHostAddress peerAddress;
      this->multicastSocket->readDatagram(datagram.data(), datagram.size(), &peerAddress);

      LOG_DEBU(this->logger, "[MULTICAST] Recived from " +  peerAddress.toString() + " message " + datagram.data());

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
            Protos::Core::IMAlive IMAlimeMessage;
            IMAlimeMessage.ParseFromString(datagram.mid(1).data());
            quint64 amount = IMAlimeMessage.amount();
            QString nick(IMAlimeMessage.nick().data());
            Common::Hash id(IMAlimeMessage.peer_id().hash().data());

            //We forward the information to the PeerManager
            this->peerManager->updatePeer(id, peerAddress, nick, amount);

            //this->logger->log("Someone is alive: " + id + ", " +data.fromStdString(IMAlimeMessage.nick()), LM::Debug);

            break;
         }

         case findPacket:
         {
            // We create a new findMessage.
            Protos::Core::Find findMessage;
            findMessage.ParseFromString(datagram.mid(1).data());

            emit newFindRequset(findMessage, peerAddress);

            //ISO C++ says that these are ambiguous, even though the worst conversion for the first is better than the worst conversion for the secondthis->logger->log("Find request id " + QString::fromStdString(findMessage.DebugString())  + QString::number( findMessage.tag() ), LM::Debug);

            break;
         }

         default:
         {
            LOG_DEBU(this->logger, "Unknow type ???");
            break;
         }
      }
   }
}

/**
 * Function called when data is recevied by the socket : The coresponding proto is created and the coresponding event is rised
 *
 * @author mcuony
 */
void UDPListener::processPendingUnicastDatagrams()
{

   QTextStream out(stdout);

   while (this->unicastSocket->hasPendingDatagrams())
   {
      QByteArray datagram;
      datagram.resize(this->unicastSocket->pendingDatagramSize());
      QHostAddress peerAddress;
      this->unicastSocket->readDatagram(datagram.data(), datagram.size(), &peerAddress);

      LOG_DEBU(this->logger, "[UNICAST] Recived from " +  peerAddress.toString() + " message " + datagram.data());

      switch (datagram.data()[0])
      {

         case findResultPacket:
         {
            // We create a new findMessage.
            Protos::Common::FindResult findRMessage;
            findRMessage.ParseFromString(datagram.mid(1).data());

            emit newFindResult(findRMessage);

            //this->logger->log("Someone is alive: " + id + ", " +data.fromStdString(IMAlimeMessage.nick()), LM::Debug);

            break;
         }




         default:
         {
            LOG_DEBU(this->logger, "Unknow type ???");
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
bool UDPListener::sendMessage(const QByteArray& datagram)
{
   //this->logger->log("Sending " + mess, LM::Debug);

   //QByteArray datagram = mess.toUtf8();

   if (this->multicastSocket->writeDatagram(
      datagram.data(),
      datagram.size(),
      UDPListener::multicastIP,
      UDPListener::multicastPort
      ) == -1)
   {
      LOG_FATA(this->logger, "Unable to send datagram");
      return false;
   }
   else
   {
      return true;
   }
}

/**
  * Send an Udp multicast message
  *
  * @param mess : The message to send
  * @author mcuony
  */
bool UDPListener::sendMessageTo(const QByteArray& datagram, const QHostAddress& ipTo)
{
   //this->logger->log("Sending " + mess, LM::Debug);

   //QByteArray datagram = mess.toUtf8();

   if (this->unicastSocket->writeDatagram(
      datagram.data(),
      datagram.size(),
      ipTo,
      UDPListener::unicastPort
      ) == -1)
   {
      LOG_FATA(this->logger, "Unable to send datagram");
      return false;
   }
   else
   {
      return true;
   }
}
