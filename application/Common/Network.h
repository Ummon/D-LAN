#ifndef COMMON_NETWORK_H
#define COMMON_NETWORK_H

#include <QIODevice>

#include <Common/Hash.h>

namespace Common
{
   struct MessageHeader
   {
      quint32 type;
      quint32 size;
      Common::Hash senderID;
   };

   class Network
   {
   public:
      static MessageHeader readHeader(QIODevice& device, bool skipReadData = true);
   };
}

#endif
