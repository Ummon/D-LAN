#ifndef NETWORKMANAGER_SEARCH_H
#define NETWORKMANAGER_SEARCH_H

#include <ISearch.h>
#include <Common/LogManager/ILogger.h>
#include <QSharedPointer>
#include <Protos/core_protocol.pb.h>
#include <Core/PeerManager/IPeerManager.h>

namespace NL
{
   class UDPListener;

   class Search : public ISearch
   {
      Q_OBJECT
   public:
      Search(UDPListener* newUdpListener, QSharedPointer<PM::IPeerManager> newPeerManager);
      bool search(const QString& words);

   signals:
      void found(const Protos::Common::FindResult& result);

   private:
      quint64 tag;
      UDPListener* udpListener;
      bool searchLaunched;
      QDateTime dateOfLaunch;
      QSharedPointer<LM::ILogger> logger;
      QSharedPointer<PM::IPeerManager> peerManager;

   public slots:
      void newFindResult(const Protos::Common::FindResult& result);

   };
}
#endif
