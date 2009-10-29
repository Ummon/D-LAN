#ifndef NETWORKMANAGER_NETWORKLISTENER_H
#define NETWORKMANAGER_NETWORKLISTENER_H

#include <QList>
#include <QSharedPointer>
#include <QTimer>
#include <QObject>
#include <QtNetwork/QNetworkInterface>

#include <Common/LogManager/ILogger.h>
#include <Core/PeerManager/IPeerManager.h>

#include <INetworkListener.h>
#include <ISearch.h>
#include <priv/Search.h>
#include <priv/UDPListener.h>
#include <priv/TCPListener.h>

namespace NL
{
   class ChunkUpdater;
   class TCPListener;
   class UDPListener;
   class Chat;
   class Search;
   class ISearch;

   class NetworkListener : public INetworkListener
   {
      Q_OBJECT
   public:
      NetworkListener(QSharedPointer<PM::IPeerManager> newPeerManager);
      IChat* getChat();
      ISearch* search();

   private:
      ChunkUpdater* chunkUpdater;
      TCPListener* tcpListener;
      UDPListener* udpListener;

      Chat* chat;
      QTimer *timer;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<LM::ILogger> logger;

      const static double IMAliveFrequency = 0.1; ///< The message IMAlive is sent each 10s.

   public slots:
      void presence();
      void newFindRequset(const Protos::Core::Find& request, const QHostAddress& peerAdress);

   };
}
#endif
