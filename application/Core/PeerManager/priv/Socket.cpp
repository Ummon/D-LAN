#include <priv/Socket.h>
using namespace PM;

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/Network.h>
#include <Common/ZeroCopyStreamQIODevice.h>

#include <priv/Log.h>
#include <priv/PeerManager.h>

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, QTcpSocket* socket)
   : peerManager(peerManager), fileManager(fileManager), socket(socket), idle(true)
{
}

Socket::Socket(const QHostAddress& address, quint16 port)
   : idle(true)
{
   this->socket = new QTcpSocket();
   this->socket->connectToHost(address, port);
}

Socket::~Socket()
{
   delete this->socket;
}

QIODevice* Socket::getDevice()
{
   return this->socket;
}

void Socket::startListening()
{
   connect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   connect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   if (!this->socket->isValid())
      this->disconnect();
   else
      this->dataReceived();
}

bool Socket::isIdle()
{
   return this->idle;
}

void Socket::setActive()
{
   this->idle = false;
}

void Socket::send(quint32 type, const google::protobuf::Message& message)
{
   Common::MessageHeader header(type, message.ByteSize(), this->peerManager->getID());

   L_DEBU(QString("Socket::send : header.type = %1, header.size = %2\n%3").arg(header.type, 0, 16).arg(header.size).arg(QString::fromStdString(message.DebugString())));

   Common::Network::writeHeader(*this->socket, header);
   Common::ZeroCopyOutputStreamQIODevice outputStream(this->socket);
   if (!message.SerializeToZeroCopyStream(&outputStream))
      L_WARN(QString("Unable to send %1").arg(QString::fromStdString(message.DebugString())));
}

void Socket::dataReceived()
{
   try
   {
      Common::MessageHeader header = Common::Network::readHeader(*this->socket);

      L_DEBU(QString("Data received from %1 - %2, type = %3, size = %4").arg(this->socket->peerAddress().toString()).arg(header.senderID.toStr()).arg(header.type, 0, 16).arg(header.size));

      L_DEBU(QString("this->socket->bytesAvailable() = %1").arg(this->socket->bytesAvailable()));

      if (this->socket->bytesAvailable() >= header.size)
      {
         switch (header.type)
         {
         case 0x31 : // GetEntries.
            {
               Protos::Core::GetEntries getEntries;
               Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
               if (header.size == 0 || getEntries.ParseFromBoundedZeroCopyStream(&inputStream, header.size))
               {
                  this->send(
                     0x32,
                     getEntries.has_dir() ? this->fileManager->getEntries(getEntries.dir()) : this->fileManager->getEntries()
                  );
               }
               this->finished();
            }
            break;
//         case 0x32 :

         case 0x41 : // GetHashes.
            {
               Protos::Core::GetHashes getHashes;
               Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
               if (getHashes.ParseFromBoundedZeroCopyStream(&inputStream, header.size))
               {
                  this->currentHashesResult = this->fileManager->getHashes(getHashes.file());
                  connect(this->currentHashesResult.data(), SIGNAL(nextHash(Common::Hash)), this, SLOT(nextAskedHash(Common::Hash)));
                  Protos::Core::GetHashesResult res = this->currentHashesResult->start();

                  this->nbHashToSend = res.nb_hash();

                  this->send(0x42, res);

                  if (res.status() != Protos::Core::GetHashesResult_Status_OK)
                  {
                     this->currentHashesResult.clear();
                     this->finished();
                  }
               }
            }
            break;
//         case 0x42 :
//         case 0x43 :
//         case 0x44 :

         case 0x51 : // GetChunk.
            {
               Protos::Core::GetChunk getChunk;
               Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
               if (getChunk.ParseFromZeroCopyStream(&inputStream))
               {
                  this->peerManager->onGetChunk(Common::Hash(getChunk.chunk().hash().data()), getChunk.offset(), this);
               }
            }
//         case 0x52 :
         }
      }
   }
   catch (Common::notEnoughData&)
   {
      L_DEBU("Socket : Not enough data to read the header");
   }
}

void Socket::finished()
{
   this->idle = true;
   emit getIdle(this);
}

void Socket::disconnected()
{
   emit close();
}

void Socket::nextAskedHash(Common::Hash hash)
{
   Protos::Common::Hash hashProto;
   hashProto.set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   this->send(0x43, hashProto);

   if (--this->nbHashToSend == 0)
   {
      this->currentHashesResult.clear();
      this->finished();
   }
}
