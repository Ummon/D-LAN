#ifndef PEERMANAGER_GET_ENTRIES_RESULT_H
#define PEERMANAGER_GET_ENTRIES_RESULT_H

#include <QObject>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Network.h>

#include <IGetEntriesResult.h>
#include <priv/Socket.h>

namespace PM
{
   class GetEntriesResult : public IGetEntriesResult
   {
      Q_OBJECT
   public:
      GetEntriesResult(const Protos::Core::GetEntries& dir, QSharedPointer<Socket> socket);
      ~GetEntriesResult();
      void start();

   private slots:
      void newMessage(Common::Network::CoreMessageType type, const google::protobuf::Message& message);

   private:
      const Protos::Core::GetEntries dir;
      QSharedPointer<Socket> socket;
   };
}

#endif
