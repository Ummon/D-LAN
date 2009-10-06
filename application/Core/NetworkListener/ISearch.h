#ifndef NETWORKMANAGER_ISEARCH_H
#define NETWORKMANAGER_ISEARCH_H

#include <QObject>
#include <QString>

#include <Protos/common.pb.h>

namespace NetworkListener
{
   class ISearch : QObject
   {
      Q_OBJECT
   public:
      virtual ~ISearch() {}

      virtual void search(const QString& words) = 0;

   signals:
      void found(const Protos::Common::FindResult& result, quint32 IP);
   };
}
#endif
