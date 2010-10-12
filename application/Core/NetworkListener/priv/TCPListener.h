#ifndef NETWORKMANAGER_TCPLISTENER_H
#define NETWORKMANAGER_TCPLISTENER_H

#include <QObject>
#include <QList>
#include <QTcpServer>
#include <QSharedPointer>

#include <Core/PeerManager/IPeerManager.h>

namespace NL
{
   class TCPListener : QObject
   {
      Q_OBJECT
      static const int MAX_LISTEN_ATTEMPT;

   public:
      TCPListener(QSharedPointer<PM::IPeerManager> peerManager);
      quint16 getCurrentPort();

   private slots:
      void newConnection();

   private:
      QSharedPointer<PM::IPeerManager> peerManager;
      QTcpServer tcpServer;

      quint16 currentPort;

        // TODO : count the number of connection per ip per second and
        // banned temporarely an ip with too much attempt.
//      struct BannedIPs
//      {
//         QHostAddress address;
//         QDateTime time;
//      };
//      QList<BannedIPs> bannedIPs;
   };
}
#endif
