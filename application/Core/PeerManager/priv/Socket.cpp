#include <priv/Socket.h>
using namespace PM;

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>

#include <priv/Log.h>
#include <priv/PeerManager.h>
#include <priv/Constants.h>

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID, QTcpSocket* socket)
   : peerManager(peerManager), fileManager(fileManager), peerID(peerID), socket(socket), idle(false), listening(false)
{
#ifdef DEBUG
   this->num = ++Socket::currentNum;
   L_DEBU(QString("New Socket (from a QTcpSocket) [%1]").arg(this->num));
#endif

   this->socket->setReadBufferSize(SOCKET_BUFFER_SIZE);
   connect(this->socket, SIGNAL(disconnected()), this->socket, SLOT(deleteLater()));
   this->initActivityTimer();
}

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID, const QHostAddress& address, quint16 port)
   : peerManager(peerManager), fileManager(fileManager), peerID(peerID), idle(false), listening(false)
{   
#ifdef DEBUG
   this->num = ++Socket::currentNum;
   L_DEBU(QString("New Socket (connection) [%1]").arg(this->num));
#endif

   this->socket = new QTcpSocket();
   this->socket->setReadBufferSize(SOCKET_BUFFER_SIZE);
   connect(this->socket, SIGNAL(disconnected()), this->socket, SLOT(deleteLater()));
   this->socket->connectToHost(address, port);
   this->initActivityTimer();
}

Socket::~Socket()
{
   L_DEBU(QString("Socket deleted [%1]").arg(this->num));

   delete this->socket;
}

QIODevice* Socket::getDevice() const
{
   return this->socket;
}

Common::Hash Socket::getPeerID() const
{
   return this->peerID;
}

void Socket::startListening()
{
   // To prevent multi listening.
   if (this->listening)
      return;

   L_DEBU(QString("Socket starting to listen [%1]").arg(this->num));

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
   L_DEBU(QString("Socket stopping to listen [%1]").arg(this->num));

   disconnect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   disconnect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));
   this->listening = false;
}

bool Socket::isIdle()
{
   return this->idle;
}

void Socket::setActive()
{
   L_DEBU(QString("Socket set to active [%1]").arg(this->num));

   this->idle = false;
   this->activityTimer.start();
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
   while (!this->socket->atEnd())
   {
      this->setActive();

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

         if (this->currentHeader.senderID != this->peerID)
         {
            L_DEBU(QString("Peer ID from message (%1) doesn't match the known peer ID (%2)").arg(this->currentHeader.senderID.toStr()).arg(this->peerID.toStr()));
            this->finished();
            return;
         }
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
   L_DEBU(QString("Socket set to idle [%1]").arg(this->num));

   this->idle = true;
   this->socket->readAll(); // Maybe there is some garbage data..
   this->startListening();
   emit getIdle(this);
}

void Socket::close()
{
   L_DEBU(QString("Socket closed [%1]").arg(this->num));

   this->socket->close();
}

void Socket::disconnected()
{   
   L_DEBU(QString("Socket disconnected [%1]").arg(this->num));

   emit closed(this);
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
            connect(this->currentHashesResult.data(), SIGNAL(nextHash(Common::Hash)), this, SLOT(nextAskedHash(Common::Hash)), Qt::QueuedConnection);
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
      readOK = false;
   }

   return readOK;
}

void Socket::initActivityTimer()
{
   this->activityTimer.setSingleShot(true);
   this->activityTimer.setInterval(IDLE_SOCKET_TIMEOUT * 1000);
   connect(&this->activityTimer, SIGNAL(timeout()), this, SLOT(close()));
   this->activityTimer.start();
}

#ifdef DEBUG
   int Socket::currentNum(0);
#endif
