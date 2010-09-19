#include <Common/Network.h>
using namespace Common;

#include <QDataStream>

MessageHeader Network::readHeader(QIODevice& device, bool skipReadData)
{
   MessageHeader header;

   char data[HEADER_SIZE];
   qint64 length = skipReadData ? device.read(data, HEADER_SIZE) : device.peek(data, HEADER_SIZE);

   if (length == HEADER_SIZE)
   {
      QDataStream stream(QByteArray(data, HEADER_SIZE));
      stream >> header.type;
      stream >> header.size;
      stream >> header.senderID;
   }

   return header;
}

void Network::writeHeader(QIODevice& device, const MessageHeader& header)
{
   QDataStream stream(&device);
   stream << header.type;
   stream << header.size;
   stream << header.senderID;
}
