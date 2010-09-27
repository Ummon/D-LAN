#include <priv/Socket.h>
using namespace PM;

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>

#include <priv/Log.h>
#include <priv/PeerManager.h>

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, QTcpSocket* socket)
   : peerManager(peerManager), fileManager(fileManager), socket(socket), idle(true), listening(false)
{   
   connect(this->socket, SIGNAL(disconnected()), this->socket, SLOT(deleteLater()));
}

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const QHostAddress& address, quint16 port)
   : peerManager(peerManager), fileManager(fileManager), idle(true), listening(false)
{
   this->socket = new QTcpSocket();
   connect(this->socket, SIGNAL(disconnected()), this->socket, SLOT(deleteLater()));
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
   // To prevent multi listening.
   if (this->listening)
      return;

   connect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   connect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));
   this->listening = true;

   if (!this->socket->isValid())
      this->disconnect();
   else
      this->dataReceived();
}

void Socket::stopListening()
{
   disconnect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   disconnect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));
   this->listening = false;
}

bool Socket::isIdle()
{
   return this->idle;
}

void Socket::send(quint32 type, const google::protobuf::Message& message)
{
   this->setActive();
   Common::MessageHeader header(type, message.ByteSize(), this->peerManager->getID());

   L_DEBU(QString("Socket::send : header.type = %1, header.size = %2\n%3").arg(header.type, 0, 16).arg(header.size).arg(QString::fromStdString(message.DebugString())));

   Common::Network::writeHeader(*this->socket, header);
   Common::ZeroCopyOutputStreamQIODevice outputStream(this->socket);
   if (!message.SerializeToZeroCopyStream(&outputStream))
      L_WARN(QString("Unable to send %1").arg(QString::fromStdString(message.DebugString())));
}

void Socket::dataReceived()
{
   this->setActive();
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
   this->startListening();
   emit getIdle(this);
}

void Socket::close()
{
   this->socket->close();
}

void Socket::disconnected()
{
   emit closed();
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

void Socket::setActive()
{
   this->idle = false;
}

/**
  * @return 'true' if the message has been handled properly otherwise return 'false'.
  */
bool Socket::readMessage()
{
   bool readOK = false;

   switch (this->currentHeader.type)
   {
   case 0x31 : // GetEntries.
      {
         Protos::Core::GetEntries getEntries;

         // This scope (and the others ones below) is here to force the input stream to read all the bytes.
         // See Common::ZeroCopyInputStreamQIODevice::~ZeroCopyInputStreamQIODevice.
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = this->currentHeader.size == 0 || getEntries.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
            this->send(
               0x32,
               getEntries.has_dir() ? this->fileManager->getEntries(getEntries.dir()) : this->fileManager->getEntries()
            );

         this->finished();
      }
      break;
   case 0x32 : // GetEntriesResult.
      {
         Protos::Core::GetEntriesResult getEntriesResult;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getEntriesResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            emit newMessage(this->currentHeader.type, getEntriesResult);
         }
         this->finished();
      }
      break;

   case 0x41 : // GetHashes.
      {
         Protos::Core::GetHashes getHashes;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getHashes.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
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
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getHashesResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            this->nbHash = getHashesResult.nb_hash();
            emit newMessage(this->currentHeader.type, getHashesResult);
         }
      }
      break;
   case 0x43 : // Common.Hash.
      {
         Protos::Common::Hash hash;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = hash.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
         {
            emit newMessage(this->currentHeader.type, hash);
            if (--this->nbHash == 0)
               this->finished();
         }
      }
      break;

//         case 0x44 : // TODO

   case 0x51 : // GetChunk.
      {
         Protos::Core::GetChunk getChunk;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getChunk.ParseFromZeroCopyStream(&inputStream);
         }

         if (readOK)
            this->peerManager->onGetChunk(Common::Hash(getChunk.chunk().hash().data()), getChunk.offset(), this);
      }
      break;

   case 0x52 : // GetChunkResult.
      {
         Protos::Core::GetChunkResult getChunkResult;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getChunkResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
            emit newMessage(this->currentHeader.type, getChunkResult);
      }
      break;

   default:
      return false;
   }

   return readOK;
}
