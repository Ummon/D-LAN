#ifndef PEERMANAGER_ISOCKET_H
#define PEERMANAGER_ISOCKET_H

#include <QAbstractSocket>

#include <Common/Hash.h>

#include <google/protobuf/message.h>

namespace PM
{
   class ISocket
   {
   public:
      virtual ~ISocket() {}

      virtual QAbstractSocket* getQSocket() const = 0;
      virtual Common::Hash getPeerID() const = 0;
      virtual void send(quint32 type, const google::protobuf::Message& message) = 0;
      virtual void finished(bool error = false) = 0;
   };
}

#endif
