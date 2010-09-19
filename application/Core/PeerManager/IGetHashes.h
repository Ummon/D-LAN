#ifndef PEERMANAGER_IGETHASHES_H
#define PEERMANAGER_IGETHASHES_H

#include <QObject>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

namespace FM
{
   class IGetHashes : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IGetHashes() {}
      virtual void start() = 0;

   signals:
      void nextHash(Common::Hash hash);
      //void error(/*QString message*/);
      //void result(Protos::Core::GetHashesResult& result);
   };
}
#endif
