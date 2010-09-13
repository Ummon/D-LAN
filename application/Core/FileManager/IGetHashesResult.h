#ifndef FILEMANAGER_IGETHASHESRESULT_H
#define FILEMANAGER_IGETHASHESRESULT_H

#include <QObject>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

namespace FM
{
   class IGetHashesResult : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IGetHashesResult() {}
      virtual Protos::Core::GetHashesResult start() = 0;

   signals:
      void nextHash(Common::Hash hash);
      //void error(/*QString message*/);
      //void result(Protos::Core::GetHashesResult& result);
   };
}
#endif
