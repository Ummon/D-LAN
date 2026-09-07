/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <priv/NetworkListener.h>
using namespace NL;

// #include <QNetworkInformation>

#include <Common/LogManager/Builder.h>
#include <Common/Settings.h>
#include <limits>

#include <priv/Search.h>
#include <priv/Utils.h>

LOG_INIT_CPP(NetworkListener)

NetworkListener::NetworkListener(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<UM::IUploadManager> uploadManager,
   QSharedPointer<DM::IDownloadManager> downloadManager
) :
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   tCPListener(peerManager),
   uDPListener(fileManager, peerManager, uploadManager, downloadManager)
{
   // TODO: rebind the sockets automatically when the network configuration changes ('QNetworkConfigurationManager' no longer exists in Qt 6).
   connect(&this->uDPListener, &UDPListener::received, this, &NetworkListener::received);
   connect(&this->uDPListener, &UDPListener::IMAliveMessageToBeSend, this, &NetworkListener::IMAliveMessageToBeSend);

   this->rebindSockets();
}

NetworkListener::~NetworkListener()
{
   this->uDPListener.send(Common::MessageHeader::CORE_GOODBYE);
   L_DEBU("NetworkListener deleted");
}

QSharedPointer<ISearch> NetworkListener::newSearch()
{
   return QSharedPointer<ISearch>(new Search(this->uDPListener));
}

void NetworkListener::rebindSockets()
{
   this->uDPListener.closeSockets();
   this->tCPListener.close();
   this->peerManager->removeAllPeers();
   Utils::sanitizeListenSettings();

   const QHostAddress address = Utils::getCurrentAddressToListenTo();
   const quint32 basePort = SETTINGS.get<quint32>("unicast_base_port");
   constexpr int MAX_LISTEN_ATTEMPTS = 10;
   auto bindBoth = [&](quint16 port) {
      if (!this->tCPListener.listen(address, port))
         return false;
      if (this->uDPListener.bindUnicastSocket(address, this->tCPListener.getCurrentPort()))
         return true;
      this->tCPListener.close();
      return false;
   };

   bool bound = false;
   for (int n = 0; basePort != 0 && n < MAX_LISTEN_ATTEMPTS &&
        static_cast<quint64>(basePort) + n <= std::numeric_limits<quint16>::max(); ++n)
      if ((bound = bindBoth(static_cast<quint16>(basePort + n))))
         break;

   // A TCP port chosen by the OS can still be occupied by UDP. Retry a bounded number of times.
   for (int n = 0; !bound && n < MAX_LISTEN_ATTEMPTS; ++n)
      if ((bound = bindBoth(0)))
         L_WARN(QString("Listening to TCP and UDP on OS-selected port %1").arg(this->tCPListener.getCurrentPort()));

   if (!bound || !this->uDPListener.startListening())
   {
      this->uDPListener.closeSockets();
      this->tCPListener.close();
      L_ERRO(QString("Unable to initialize network listeners on %1; discovery is disabled").arg(address.toString()));
   }
}

NetworkListener::SendStatus NetworkListener::send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message, const Common::Hash& peerID)
{
   if (peerID.isNull())
      return this->uDPListener.send(type, message);
   else
      return this->uDPListener.send(type, message, peerID);
}
