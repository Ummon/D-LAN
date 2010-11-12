#ifndef PEERMANAGER_IGET_HASHES_H
#define PEERMANAGER_IGET_HASHES_H

#include <QObject>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Timeoutable.h>
#include <Common/Hash.h>

namespace PM
{
   class IGetHashesResult : public Common::Timeoutable
   {
      Q_OBJECT
   protected:
      IGetHashesResult(int time) : Common::Timeoutable(time) {}

   public:
      virtual ~IGetHashesResult() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::Core::GetHashesResult&);
      void nextHash(const Common::Hash&);
   };
}
#endif
