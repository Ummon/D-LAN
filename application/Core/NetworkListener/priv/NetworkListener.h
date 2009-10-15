#ifndef NETWORKMANAGER_NETWORKLISTENER_H
#define NETWORKMANAGER_NETWORKLISTENER_H

#include <QList>
#include <QSharedPointer>
#include <QTimer>
#include <QObject>

#include <Common/LogManager/ILogger.h>
#include <Core/PeerManager/IPeerManager.h>

#include <INetworkListener.h>
#include <priv/UDPListener.h>

namespace NetworkListener
{
   class ChunkUpdater;
   class TCPListener;
   class UDPListener;
   class Chat;
   class Search;

   class NetworkListener : public INetworkListener
   {

   public:
      NetworkListener(QSharedPointer<PeerManager::IPeerManager> newPeerManager);
      IChat* getChat();

   private:
      ChunkUpdater* chunkUpdater;
      TCPListener* tcpListener;
      UDPListener* udpListener;

      Chat* chat;
      QList<Search*> searches;
      QTimer *timer;
      QSharedPointer<PeerManager::IPeerManager> peerManager;
      QSharedPointer<LogManager::ILogger> logger;

      const static double IMAliveFrequency = 0.1; ///< The message IMAlive is sent each 10s.

   public slots:
      void presence();

   };
}
#endif
