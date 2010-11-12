#ifndef PEERMANAGER_IGET_ENTRIES_RESULT_H
#define PEERMANAGER_IGET_ENTRIES_RESULT_H

#include <QObject>

#include <Protos/core_protocol.pb.h>

#include <Common/Timeoutable.h>

namespace PM
{
   class IGetEntriesResult : public Common::Timeoutable
   {
      Q_OBJECT
   protected:
      IGetEntriesResult(int time) : Common::Timeoutable(time) {}

   public:
      virtual ~IGetEntriesResult() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::Common::Entries& entries);
   };
}
#endif
