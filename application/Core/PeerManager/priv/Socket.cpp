#include <priv/Socket.h>
using namespace PM;

#include <QCoreApplication>

#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>
#include <Common/ProtoHelper.h>

#include <priv/Log.h>
#include <priv/PeerManager.h>
#include <priv/Constants.h>

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID, QTcpSocket* socket)
   : peerManager(peerManager), fileManager(fileManager), peerID(peerID), socket(socket), idle(false), listening(false), nbError(0)
{
#ifdef DEBUG
   this->num = ++Socket::currentNum;
   L_DEBU(QString("New Socket[%1] (connection from %2:%3)").arg(this->num).arg(socket->peerAddress().toString()).arg(socket->peerPort()));
#endif

   this->socket->setReadBufferSize(SETTINGS.get<quint32>("socket_buffer_size"));
   this->initActivityTimer();
}

Socket::Socket(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID, const QHostAddress& address, quint16 port)
   : peerManager(peerManager), fileManager(fileManager), peerID(peerID), idle(false), listening(false), nbError(0)
{   
#ifdef DEBUG
   this->num = ++Socket::currentNum;
   L_DEBU(QString("New Socket[%1] (connection to %2:%3)").arg(this->num).arg(address.toString()).arg(port));
#endif

   this->socket = new QTcpSocket();
   this->socket->setReadBufferSize(SETTINGS.get<quint32>("socket_buffer_size"));
   this->socket->connectToHost(address, port);
   this->initActivityTimer();
}

Socket::~Socket()
{
   L_DEBU(QString("Socket[%1] deleted").arg(this->num));

   disconnect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
   disconnect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));

   this->socket->deleteLater();
}

QAbstractSocket* Socket::getQSocket() const
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

   L_DEBU(QString("Socket[%1] starting to listen").arg(this->num));

   this->listening = true;

   if (!this->socket->isValid())
      this->close();
   else
   {
      connect(this->socket, SIGNAL(readyRead()), this, SLOT(dataReceived()));
      connect(this->socket, SIGNAL(disconnected()), this, SLOT(disconnected()));
      this->dataReceived();
   }
}

void Socket::stopListening()
{
   L_DEBU(QString("Socket[%1] stopping to listen").arg(this->num));

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
   if (!this->idle)
      return;

   L_DEBU(QString("Socket[%1] set to active >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>").arg(this->num));

   this->idle = false;
   this->activityTimer.start();
}

void Socket::send(quint32 type, const google::protobuf::Message& message)
{
   this->setActive();

   Common::MessageHeader header(type, message.ByteSize(), this->peerManager->getID());

   L_DEBU(QString("Socket[%1]::send : header.type = %2, header.size = %3\n%4").arg(this->num).arg(header.type, 0, 16).arg(header.size).arg(Common::ProtoHelper::getDebugStr(message)));

   Common::Network::writeHeader(*this->socket, header);
   Common::ZeroCopyOutputStreamQIODevice outputStream(this->socket);
   if (!message.SerializeToZeroCopyStream(&outputStream))
      L_WARN(QString("Unable to send %1").arg(Common::ProtoHelper::getDebugStr(message)));
}

void Socket::dataReceived()
{
   // TODO : it will loop infinetly if not enough data is provided.
   while (!this->socket->atEnd() && this->listening)
   {
      QCoreApplication::processEvents(); // To read from the native socket to the internal QTcpSocket buffer. TODO : more elegant way?
      this->setActive();

      //L_DEBU(QString("Socket[%1]::dataReceived() : bytesAvailable = %2").arg(this->num).arg(this->socket->bytesAvailable()));

      if (this->currentHeader.isNull() && this->socket->bytesAvailable() >= Common::Network::HEADER_SIZE)
      {
         this->currentHeader = Common::Network::readHeader(*this->socket);

         L_DEBU(QString("Socket[%1]: Data received from %2 - %3, type = %4, size = %5")
            .arg(this->num)
            .arg(this->socket->peerAddress().toString())
            .arg(this->currentHeader.senderID.toStr())
            .arg(this->currentHeader.type, 0, 16)
            .arg(this->currentHeader.size)
         );

         if (this->currentHeader.senderID != this->peerID)
         {
            L_DEBU(QString("Socket[%1]: Peer ID from message (%2) doesn't match the known peer ID (%3)").arg(this->num).arg(this->currentHeader.senderID.toStr()).arg(this->peerID.toStr()));
            this->finished(true);
            this->currentHeader.setNull();
            return;
         }
      }

      if (!this->currentHeader.isNull() && this->socket->bytesAvailable() >= this->currentHeader.size)
      {
         if (!this->readMessage())
         {
            L_DEBU("Can't read the message -> finished");
            this->finished(true);
         }
         this->currentHeader.setNull();
      }
   }
}

void Socket::finished(bool error)
{
   if (this->idle)
      return;

   L_DEBU(QString("Socket[%1] set to idle%2<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<").arg(this->num).arg(error ? " with error " : " "));

   if (error && ++this->nbError > 5) // TODO : -> constant
   {
      L_WARN("Socket with too many error, closed");
      this->close();
      return;
   }

   if (!this->socket->isValid())
   {
      this->close();
      return;
   }

   this->idle = true;

   this->startListening();
   emit getIdle(this);
}

/**
  * Close remove from the connection pool.
  */
void Socket::close()
{
   L_DEBU(QString("Socket[%1] closing..").arg(this->num));

   this->idle = true;
   emit closed(this);
}

void Socket::disconnected()
{   
   L_DEBU(QString("Socket[%1] disconnected").arg(this->num));
   this->close();
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
         Protos::Common::Entries getEntriesResult;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getEntriesResult.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         this->finished();

         if (readOK)
         {
            emit newMessage(this->currentHeader.type, getEntriesResult);
         }
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
            if (--this->nbHash == 0)
               this->finished();

            emit newMessage(this->currentHeader.type, hash);
         }
      }
      break;

//         case 0x44 : // TODO

   case 0x51 : // GetChunk.
      {
         Protos::Core::GetChunk getChunkMessage;
         {
            Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
            readOK = getChunkMessage.ParseFromBoundedZeroCopyStream(&inputStream, this->currentHeader.size);
         }

         if (readOK)
            emit getChunk(Common::Hash(getChunkMessage.chunk().hash().data()), getChunkMessage.offset(), this);
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
   this->activityTimer.setInterval(SETTINGS.get<quint32>("idle_socket_timeout"));
   connect(&this->activityTimer, SIGNAL(timeout()), this, SLOT(close()));
   this->activityTimer.start();
}

#ifdef DEBUG
   int Socket::currentNum(0);
#endif
