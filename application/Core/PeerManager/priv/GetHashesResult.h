#ifndef PEERMANAGER_GET_HASHES_RESULT_H
#define PEERMANAGER_GET_HASHES_RESULT_H

#include <QObject>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <IGetHashesResult.h>
#include <priv/Socket.h>

namespace PM
{
   class GetHashesResult : public IGetHashesResult
   {
      Q_OBJECT
   public:
      GetHashesResult(const Protos::Common::Entry& file, Socket* socket);
      ~GetHashesResult();
      void start();

   private slots:
      void newMessage(quint32 type, const google::protobuf::Message& message);

   private:
      const Protos::Common::Entry file;
      Socket* socket;
   };
}

#endif
