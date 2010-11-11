#ifndef PEERMANAGER_IGET_CHUNK_RESULT_H
#define PEERMANAGER_IGET_CHUNK_RESULT_H

#include <QObject>
#include <QIODevice>
#include <QSharedPointer>

#include <Protos/core_protocol.pb.h>

#include <Common/Timeoutable.h>

#include <Core/PeerManager/ISocket.h>

namespace PM
{
   class IGetChunkResult : public Common::Timeoutable
   {
      Q_OBJECT
   protected:
      IGetChunkResult(int time) : Common::Timeoutable(time) {}

   public:
      virtual ~IGetChunkResult() {}
      virtual void start() = 0;

      /**
        * If there is an error during the streaming, it can reported by calling this method.
        */
      virtual void setError() = 0;

   signals:
      void result(const Protos::Core::GetChunkResult& result);
      void stream(QSharedPointer<PM::ISocket> socket);
   };
}
#endif
