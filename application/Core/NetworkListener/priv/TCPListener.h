#ifndef NETWORKMANAGER_TCPLISTENER_H
#define NETWORKMANAGER_TCPLISTENER_H

#include <QTcpSocket>
#include <QHostAddress>
#include <QTcpServer>
#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>
#include <Core/PeerManager/IPeerManager.h>

namespace NetworkListener
{
   class TCPListener : public QTcpServer
   {
   Q_OBJECT

   public:
      TCPListener(QSharedPointer<PeerManager::IPeerManager> newPeerManager);
   private:
      QSharedPointer<LM::ILogger> logger;
      QSharedPointer<PeerManager::IPeerManager> peerManager;

   public slots:
      void newConnexion();
   };
}
#endif
