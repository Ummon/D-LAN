#ifndef PEERMANAGER_IGET_ENTRIES_RESULT_H
#define PEERMANAGER_IGET_ENTRIES_RESULT_H

#include <QObject>

#include <Protos/core_protocol.pb.h>

namespace PM
{
   class IGetEntriesResult : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IGetEntriesResult() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::Core::GetEntriesResult& entries);
   };
}
#endif
