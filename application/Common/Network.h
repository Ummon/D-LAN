#ifndef COMMON_NETWORK_H
#define COMMON_NETWORK_H

#include <QIODevice>

#include <Common/Hash.h>

namespace Common
{
   class notEnoughData {};

   struct MessageHeader
   {
      quint32 type;
      quint32 size;
      Common::Hash senderID;
   };

   class Network
   {
   public:
      static const int HEADER_SIZE = sizeof(MessageHeader::type) + sizeof(MessageHeader::size) + Hash::HASH_SIZE;

      static MessageHeader readHeader(QIODevice& device, bool skipReadData = true);
      static void writeHeader(QIODevice& device, const MessageHeader& header);
   };
}

#endif
