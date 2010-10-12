#include <Common/Network.h>
using namespace Common;

MessageHeader Network::readHeader(QIODevice& device, bool skipReadData)
{
   if (device.bytesAvailable() < HEADER_SIZE)
      throw notEnoughData();

   char data[HEADER_SIZE];

   if (skipReadData)
      device.read(data, HEADER_SIZE);
   else
      device.peek(data, HEADER_SIZE);

   return Network::readHeader(data);
}

/**
  * @remarks The buffer size must be at least the header size (28 bytes).
  */
MessageHeader Network::readHeader(const char* data)
{
   MessageHeader header;

   QDataStream stream(QByteArray(data, HEADER_SIZE));
   stream >> header.type;
   stream >> header.size;
   stream >> header.senderID;

   return header;
}

void Network::writeHeader(QIODevice& device, const MessageHeader& header)
{
   QDataStream stream(&device);
   Network::writeHeader(stream, header);
}

void Network::writeHeader(char* buffer, const MessageHeader& header)
{
   QDataStream stream(QByteArray(buffer, HEADER_SIZE));
   Network::writeHeader(stream, header);
}

inline void Network::writeHeader(QDataStream& stream, const MessageHeader& header)
{
   stream << header.type;
   stream << header.size;
   stream << header.senderID;
}
