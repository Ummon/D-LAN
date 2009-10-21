#ifndef PEERMANAGER_PEERMANAGER_H
#define PEERMANAGER_PEERMANAGER_H

#include <QString>
#include <QList>
#include <QSharedPointer>
#include <QTimer>
#include <QObject>

#include <Common/Hash.h>

#include <IPeerManager.h>
#include <Common/LogManager/ILogger.h>
#include <priv/Peer.h>

//Clean up each 10s.
#define CleanUpFrequency 0.1

namespace PeerManager
{
   class Peer;

   class PeerManager : public IPeerManager
   {
       public:
          PeerManager();

          Common::Hash getMyId();
          void setNick(const QString& newNick);
          QString* getNick();
          void updatePeer(const Common::Hash& peerID, const QHostAddress&  peerIP, const QString& peerNick, const quint64& peerAmount);
          void newSocket(const QHostAddress&  peerIP, QSharedPointer<QTcpSocket> socket);

       private:
          QList<Peer*> peers;
          Common::Hash ID;
          QString nick;
          QTimer *timer;
          QSharedPointer<LM::ILogger> logger;
          Peer* fromIdToPeer(const Common::Hash& peerID);

       public slots:
          void cleanUp();
   };
}
#endif
