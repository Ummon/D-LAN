#ifndef FILEMANAGER_IGETHASHESRESULT_H
#define FILEMANAGER_IGETHASHESRESULT_H

#include <QObject>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

namespace FM
{
   class IGetHashesResult : QObject
   {
      Q_OBJECT
   public:
      virtual ~IGetHashesResult() {}
      virtual void start() = 0;

   signals:
      void result(Protos::Core::GetHashesResult& result);
   };
}
#endif
