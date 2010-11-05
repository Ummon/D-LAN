#ifndef PEERMANAGER_ISOCKET_H
#define PEERMANAGER_ISOCKET_H

#include <QAbstractSocket>

#include <Common/Hash.h>
#include <Common/Network.h>

#include <google/protobuf/message.h>

namespace PM
{
   class ISocket
   {
   public:
      virtual ~ISocket() {}

      virtual QAbstractSocket* getQSocket() const = 0;
      virtual Common::Hash getPeerID() const = 0;
      virtual void send(Common::Network::CoreMessageType type, const google::protobuf::Message& message) = 0;

      /**
        * Called before an upload or a download to take the control of the socket.
        * The listening is restored by a call to 'finished(..)'.
        */
      virtual void stopListening() = 0;
      virtual void finished(bool error = false) = 0;
   };
}

#endif
