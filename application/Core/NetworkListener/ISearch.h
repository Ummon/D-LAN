#ifndef NETWORKMANAGER_ISEARCH_H
#define NETWORKMANAGER_ISEARCH_H

#include <QObject>
#include <QString>

#include <Protos/common.pb.h>

namespace NetworkListener
{
   class ISearch : public QObject
   {
   Q_OBJECT

   public:
      virtual ~ISearch() {}
      virtual bool search(const QString& words) = 0;

   signals:
      void found(const Protos::Common::FindResult& result, quint32 IP);

   public slots:
      virtual void newFindResult(const Protos::Common::FindResult& result) = 0;
   };
}
#endif
