#ifndef PEERMANAGER_IGETENTRIES_H
#define PEERMANAGER_IGETENTRIES_H

#include <QObject>

#include <Protos/core_protocol.pb.h>

namespace PeerManager
{
   class IGetEntries : QObject
   {
       Q_OBJECT
       public:
          virtual ~IGetEntries() {}

       signals:
         virtual void result(const Protos::Core::GetEntriesResult& result) = 0;
   };
}
#endif
