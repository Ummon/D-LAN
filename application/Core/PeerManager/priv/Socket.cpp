#include <priv/Socket.h>
using namespace PM;

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>

#include <priv/Log.h>
#include <priv/PeerManager.h>

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, QTcpSocket* socket)
   : peerManager(peerManager), fileManager(fileManager), socket(socket), idle(true)
{
}

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const QHostAddress& address, quint16 port)
   : peerManager(peerManager), fileManager(fileManager), idle(true)
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
   while (this->socket->bytesAvailable())
   {
      L_DEBU(QString("Socket::dataReceived() : bytesAvailable = %1").arg(this->socket->bytesAvailable()));

      if (this->currentHeader.isNull() && this->socket->bytesAvailable() >= Common::Network::HEADER_SIZE)
      {
         this->currentHeader = Common::Network::readHeader(*this->socket);

         L_DEBU(QString("Data received from %1 - %2, type = %3, size = %4")
            .arg(this->socket->peerAddress().toString())
            .arg(this->currentHeader.senderID.toStr())
            .arg(this->currentHeader.type, 0, 16)
            .arg(this->currentHeader.size)
         );
      }

      if (!this->currentHeader.isNull() && this->socket->bytesAvailable() >= this->currentHeader.size)
      {
         if (!this->readMessage())
            this->finished();
         this->currentHeader.setNull();
      }
   }
}

void Socket::finished()
{
   this->idle = true;
   this->socket->readAll(); // Maybe there is some garbage data..
   emit getIdle(this);
}

void Socket::disconnected()
{
   emit close();
}

/**
  * When we ask to the fileManager some hashes for a given file this
  * slot will be called each time a new hash is available.
  */
void Socket::nextAskedHash(Common::Hash hash)
{
   Protos::Common::Hash hashProto;
   hashProto.set_hash(hash.getData(), Common::Hash::HASH_SIZE);
   this->send(0x43, hashProto);

   if (--this->nbHash == 0)
   {
      this->currentHashesResult.clear();
      this->finished();
   }
}

/**
  * @return 'true' if the message has been handled properly otherwise return 'false'.
  */
bool Socket::readMessage()
{
   switch (this->currentHeader.type)
   {
   case 0x31 : // GetEntries.
      {
         Protos::Core::GetEntries getEntries;
         Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
         if (this->currentHeader.size == 0 || getEntries.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size))
         {
            this->send(
               0x32,
               getEntries.has_dir() ? this->fileManager->getEntries(getEntries.dir()) : this->fileManager->getEntries()
            );
         }
         this->finished();
      }
      break;
   case 0x32 : // GetEntriesResult.
      {
         Protos::Core::GetEntriesResult getEntriesResult;
         Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
         if (getEntriesResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size))
         {
            emit newMessage(this->currentHeader.type, getEntriesResult);
         }
         this->finished();
      }
      break;

   case 0x41 : // GetHashes.
      {
         Protos::Core::GetHashes getHashes;
         Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
         if (getHashes.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size))
         {
            this->currentHashesResult = this->fileManager->getHashes(getHashes.file());
            connect(this->currentHashesResult.data(), SIGNAL(nextHash(Common::Hash)), this, SLOT(nextAskedHash(Common::Hash)));
            Protos::Core::GetHashesResult res = this->currentHashesResult->start();

            this->nbHash = res.nb_hash();

            this->send(0x42, res);

            if (res.status() != Protos::Core::GetHashesResult_Status_OK)
            {
               this->currentHashesResult.clear();
               this->finished();
            }
         }
      }
      break;
   case 0x42 : // GetHashesResult.
      {
         Protos::Core::GetHashesResult getHashesResult;
         Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
         if (getHashesResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size))
         {
            this->nbHash = getHashesResult.nb_hash();
            emit newMessage(this->currentHeader.type, getHashesResult);
         }
      }
      break;
   case 0x43 : // Common.Hash.
      {
         Protos::Common::Hash hash;
         Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
         if (hash.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size))
         {
            emit newMessage(this->currentHeader.type, hash);
            if (--this->nbHash == 0)
               this->finished();
         }
      }

//         case 0x44 : // TODO

//         case 0x51 : // GetChunk.
//            {
//               Protos::Core::GetChunk getChunk;
//               Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
//               if (getChunk.ParseFromZeroCopyStream(&inputStream))
//               {
//                  this->peerManager->onGetChunk(Common::Hash(getChunk.chunk().hash().data()), getChunk.offset(), this);
//               }
//            }
//         case 0x52 :
   default:
      return false;
   }

   return true;
}
