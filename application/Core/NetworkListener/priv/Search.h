#ifndef NETWORKMANAGER_SEARCH_H
#define NETWORKMANAGER_SEARCH_H

#include <QSharedPointer>

#include <Protos/core_protocol.pb.h>

#include <ISearch.h>
#include <priv/UDPListener.h>

namespace NL
{
   class UDPListener;

   class Search : public ISearch
   {
      Q_OBJECT
   public:
      Search(UDPListener& uDPListener);
      bool search(const QString& words);

   signals:
      void found(const Protos::Common::FindResult& result);

   private:
      quint64 tag;

      UDPListener& uDPListener;

      bool searchLaunched;
      QDateTime dateOfLaunch;
      QSharedPointer<PM::IPeerManager> peerManager;

   public slots:
      void newFindResult(const Protos::Common::FindResult& result);

   };
}
#endif
