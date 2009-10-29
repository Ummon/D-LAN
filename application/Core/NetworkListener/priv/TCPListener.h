#ifndef NETWORKMANAGER_TCPLISTENER_H
#define NETWORKMANAGER_TCPLISTENER_H

#include <QTcpSocket>
#include <QHostAddress>
#include <QTcpServer>
#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>
#include <Core/PeerManager/IPeerManager.h>

namespace NL
{
   class TCPListener : public QTcpServer
   {
   Q_OBJECT

   public:
      TCPListener(QSharedPointer<PM::IPeerManager> newPeerManager);
   private:
      QSharedPointer<LM::ILogger> logger;
      QSharedPointer<PM::IPeerManager> peerManager;

   public slots:
      void newConnexion();
   };
}
#endif
