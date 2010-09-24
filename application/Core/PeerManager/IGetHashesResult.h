#ifndef PEERMANAGER_IGET_HASHES_H
#define PEERMANAGER_IGET_HASHES_H

#include <QObject>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

namespace PM
{
   class IGetHashesResult : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IGetHashesResult() {}
      virtual void start() = 0;

   signals:
      void nextHash(Common::Hash hash);
   };
}
#endif
