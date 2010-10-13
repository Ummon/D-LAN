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

   QByteArray array = QByteArray::fromRawData(data, HEADER_SIZE);
   QDataStream stream(&array, QIODevice::ReadOnly);
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
   // We can't use 'fromRawData' because QByteArray can't modify the given buffer.
   // QByteArray array = QByteArray::fromRawData(buffer, HEADER_SIZE);

   QByteArray array(HEADER_SIZE, 0);
   QDataStream stream(&array, QIODevice::WriteOnly);
   Network::writeHeader(stream, header);
   memcpy(buffer, array.data(), HEADER_SIZE);
}

inline void Network::writeHeader(QDataStream& stream, const MessageHeader& header)
{
   stream << header.type;
   stream << header.size;
   stream << header.senderID;
}
