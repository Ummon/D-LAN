#ifndef PEERMANAGER_GET_CHUNK_RESULT_H
#define PEERMANAGER_GET_CHUNK_RESULT_H

#include <QObject>
#include <QTimer>

#include <google/protobuf/message.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Network.h>

#include <IGetChunkResult.h>
#include <priv/Socket.h>

namespace PM
{
   class GetChunkResult : public IGetChunkResult
   {
      Q_OBJECT
   public:
      GetChunkResult(const Protos::Core::GetChunk& chunk, QSharedPointer<Socket> socket);
      ~GetChunkResult();
      void start();
      void setError();

   private slots:
      void newMessage(Common::Network::CoreMessageType type, const google::protobuf::Message& message);

   private:
      const Protos::Core::GetChunk chunk;
      QSharedPointer<Socket> socket;
      bool error;
   };
}

#endif
