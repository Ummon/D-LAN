#ifndef NETWORKMANAGER_SEARCH_H
#define NETWORKMANAGER_SEARCH_H

#include <ISearch.h>
#include <Common/LogManager/ILogger.h>
#include <QSharedPointer>
#include <Protos/core_protocol.pb.h>
#include <Core/PeerManager/IPeerManager.h>

namespace NetworkListener { class UDPListener; }

namespace NetworkListener
{
   class UDPListener;

   class Search : public ISearch
   {
   public:
      Search(UDPListener* newUdpListener, QSharedPointer<PeerManager::IPeerManager> newPeerManager);
      bool search(const QString& words);

   signals:
      void found(const Protos::Common::FindResult& result);

   private:
      ::google::protobuf::uint64 tag;
      UDPListener* udpListener;
      bool searchLaunched;
      QDateTime dateOfLaunch;
      QSharedPointer<LogManager::ILogger> logger;
      QSharedPointer<PeerManager::IPeerManager> peerManager;

   public slots:
      void newFindResult(const Protos::Common::FindResult& result);

   };
}
#endif
