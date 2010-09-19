#ifndef PEERMANAGER_PEERMANAGER_H
#define PEERMANAGER_PEERMANAGER_H

#include <QObject>
#include <QString>
#include <QTimer>
#include <QList>
#include <QSharedPointer>
#include <QTcpSocket>

#include <Common/Hash.h>

#include <Core/FileManager/IFileManager.h>

#include <IPeerManager.h>
#include <Common/LogManager/ILogger.h>
#include <priv/Peer.h>

namespace PM
{
   class Peer;

   class PeerManager : public IPeerManager
   {
      Q_OBJECT
   public:
      PeerManager(QSharedPointer<FM::IFileManager> fileManager);

      Common::Hash getID();
      void setNick(const QString& nick);
      QString getNick();

      QList<IPeer*> getPeers();
      IPeer* getPeer(const Common::Hash& ID);
      Peer* getPeer_(const Common::Hash& ID);

      void updatePeer(const Common::Hash& ID, const QHostAddress& IP, const QString& nick, const quint64& sharingAmount);
      void newConnection(QSharedPointer<QTcpSocket> socket);

   private slots:
      void dataReceived(QTcpSocket* socket = 0);
      void disconnected();

   private:
      QSharedPointer<QTcpSocket> removeSocketFromPending(QTcpSocket* socket);

      QSharedPointer<FM::IFileManager> fileManager;

      Common::Hash ID;
      QString nick;
      QList<Peer*> peers;

      QTimer timer;

      QList< QSharedPointer<QTcpSocket> > pendingSockets;
   };
}
#endif
