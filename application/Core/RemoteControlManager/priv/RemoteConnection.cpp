#include <priv/RemoteConnection.h>
using namespace RCM;

#include <Protos/gui_protocol.pb.h>

#include <Core/PeerManager/IPeer.h>
#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Network.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>
#include <Common/Hash.h>

#include <priv/Log.h>

RemoteConnection::RemoteConnection(
   QSharedPointer<FM::IFileManager> fileManager,
   QSharedPointer<PM::IPeerManager> peerManager,
   QSharedPointer<UM::IUploadManager> uploadManager,
   QSharedPointer<DM::IDownloadManager> downloadManager,
   QSharedPointer<NL::INetworkListener> networkListener,
   QTcpSocket* socket
) :
   fileManager(fileManager),
   peerManager(peerManager),
   uploadManager(uploadManager),
   downloadManager(downloadManager),
   networkListener(networkListener),
   socket(socket)
{
   this->timerRefresh.setInterval(SETTINGS.get<quint32>("remote_refresh_rate"));
   connect(&this->timerRefresh, SIGNAL(timeout()), this, SLOT(refresh()));
   this->timerRefresh.start();
   this->refresh();

   connect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   connect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   if (!this->socket->isValid())
      this->disconnect();
   else
      this->dataReceived(); // The case where some data arrived before the 'connect' above.
}

RemoteConnection::~RemoteConnection()
{
   emit deleted(this);
   delete this->socket;
}

void RemoteConnection::refresh()
{
   Protos::GUI::State state;

   QList<PM::IPeer*> peers = this->peerManager->getPeers();
   for (QListIterator<PM::IPeer*> i(peers); i.hasNext();)
   {
      PM::IPeer* peer = i.next();
      Protos::GUI::State_Peer* protoPeer = state.add_peer();
      protoPeer->mutable_peer_id()->set_hash(peer->getID().getData(), Common::Hash::HASH_SIZE);
      Common::ProtoHelper::setStr(*protoPeer, &Protos::GUI::State_Peer::set_nick, peer->getNick());
      protoPeer->set_sharing_amount(peer->getSharingAmount());
   }

   this->send(0x01, state);
}

void RemoteConnection::dataReceived()
{
   if (this->socket->bytesAvailable() >= Common::Network::HEADER_SIZE)
   {
      const Common::MessageHeader& header = Common::Network::readHeader(*this->socket);
   }
}

void RemoteConnection::disconnected()
{
   L_DEBU("Connection dropped");

   this->deleteLater();
}

void RemoteConnection::send(quint32 type, const google::protobuf::Message& message)
{
   Common::MessageHeader header(type, message.ByteSize(), this->peerManager->getID());

   L_DEBU(QString("RemoteConnection::send : header.type = %1, header.size = %2\n%3").arg(header.type, 0, 16).arg(header.size).arg(Common::ProtoHelper::getDebugStr(message)));

   Common::Network::writeHeader(*this->socket, header);
   Common::ZeroCopyOutputStreamQIODevice outputStream(this->socket);
   if (!message.SerializeToZeroCopyStream(&outputStream))
      L_WARN(QString("Unable to send %1").arg(Common::ProtoHelper::getDebugStr(message)));
}
