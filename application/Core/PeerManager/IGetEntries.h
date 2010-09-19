#ifndef PEERMANAGER_IGETENTRIES_H
#define PEERMANAGER_IGETENTRIES_H

#include <QObject>

#include <Protos/core_protocol.pb.h>

namespace PM
{
   class IGetEntries : QObject
   {
   Q_OBJECT

   public:
      virtual ~IGetEntries() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::Core::GetEntriesResult& result);
   };
}
#endif
