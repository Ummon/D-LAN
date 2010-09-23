#include <priv/Socket.h>
using namespace PM;

#include <Protos/core_protocol.pb.h>

#include <Common/Network.h>
#include <Common/ZeroCopyStreamQIODevice.h>
#include <priv/Log.h>

Socket::Socket(QTcpSocket* socket)
   : socket(socket), idle(true)
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

void Socket::send(quint32 type, const google::protobuf::Message& message, const Common::Hash& senderID)
{
   Common::MessageHeader header;
   header.type = type;
   header.size = message.ByteSize();
   header.senderID = senderID;

   L_DEBU(QString("Socket::send : header.type = %1, header.size = %2").arg(header.type, 0, 16).arg(header.size));
   L_DEBU(QString::fromStdString(message.DebugString()));

   Common::Network::writeHeader(*this->socket, header);
   Common::ZeroCopyOutputStreamQIODevice outputStream(this->socket);
   message.SerializeToZeroCopyStream(&outputStream);
}

void Socket::dataReceived()
{
   try
   {
      Common::MessageHeader header = Common::Network::readHeader(*this->socket);

      L_DEBU(QString("Data received from from %1 - %2, type = %3, size = %4").arg(this->socket->peerAddress().toString()).arg(header.senderID.toStr()).arg(header.type, 0, 16).arg(header.size));

      L_DEBU(QString("this->socket->bytesAvailable() = %1").arg(this->socket->bytesAvailable()));

      if (this->socket->bytesAvailable() >= header.size)
      {
         switch (header.type)
         {
         case 0x31 :
            {
               Protos::Core::GetEntries getEntries;
               Common::ZeroCopyInputStreamQIODevice inputStream(this->socket);
               if (header.size == 0 || getEntries.ParseFromZeroCopyStream(&inputStream))
                  emit newMessage(header.type, getEntries, this);
            }
//         case 0x32 :

//         case 0x41 :
//         case 0x42 :
//         case 0x43 :
//         case 0x44 :

//         case 0x51 :
//         case 0x52 :
         }
      }
   }
   catch (Common::notEnoughData&)
   {
      L_DEBU("Socket : Not enough data for read the header");
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
