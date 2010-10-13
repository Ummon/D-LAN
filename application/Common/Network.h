#ifndef COMMON_NETWORK_H
#define COMMON_NETWORK_H

#include <QIODevice>
#include <QDataStream>

#include <Common/Hash.h>

namespace Common
{
   class notEnoughData {};

   struct MessageHeader
   {
      MessageHeader() : type(0), size(0) {}
      MessageHeader(quint32 type, quint32 size, const Common::Hash senderID) : type(type), size(size), senderID(senderID) {}

      bool isNull() const { return this->type == 0; }
      void setNull() { this->type = 0; }

      quint32 type;
      quint32 size;
      Common::Hash senderID;
   };

   class Network
   {
   public:
      static const int HEADER_SIZE = sizeof(MessageHeader::type) + sizeof(MessageHeader::size) + Hash::HASH_SIZE;

      static MessageHeader readHeader(QIODevice& device, bool skipReadData = true);
      static MessageHeader readHeader(const char* data);

      static void writeHeader(QIODevice& device, const MessageHeader& header);
      static void writeHeader(char* buffer, const MessageHeader& header);

   private:
      inline static void writeHeader(QDataStream& stream, const MessageHeader& header);
   };
}

#endif
