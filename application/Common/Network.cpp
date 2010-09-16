#include <Common/Network.h>
using namespace Common;

#include <QDataStream>

MessageHeader Network::readHeader(QIODevice& device, bool skipReadData)
{
   MessageHeader header;

   char data[sizeof(header.type) + sizeof(header.size) + Hash::HASH_SIZE];
   qint64 length = skipReadData ? device.read(data, sizeof(data)) : device.peek(data, sizeof(data));

   if (length == sizeof(data))
   {
      QDataStream stream(data);
      stream >> header.type;
      stream >> header.size;
      stream >> header.senderID;
   }

   return header;
}
