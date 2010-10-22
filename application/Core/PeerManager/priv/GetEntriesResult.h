#ifndef PEERMANAGER_GET_ENTRIES_RESULT_H
#define PEERMANAGER_GET_ENTRIES_RESULT_H

#include <QObject>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <IGetEntriesResult.h>
#include <priv/Socket.h>

namespace PM
{
   class GetEntriesResult : public IGetEntriesResult
   {
      Q_OBJECT
   public:
      GetEntriesResult(const Protos::Core::GetEntries& dir, Socket* socket);
      ~GetEntriesResult();
      void start();

   private slots:
      void newMessage(quint32 type, const google::protobuf::Message& message);

   private:
      const Protos::Core::GetEntries dir;
      Socket* socket;
   };
}

#endif
