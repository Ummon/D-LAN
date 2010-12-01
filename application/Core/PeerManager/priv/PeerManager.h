#ifndef PEERMANAGER_PEERMANAGER_H
#define PEERMANAGER_PEERMANAGER_H

#include <QObject>
#include <QString>
#include <QTimer>
#include <QTime>
#include <QList>
#include <QTcpSocket>

#include <Common/Hash.h>
#include <Common/Uncopyable.h>

#include <Core/FileManager/IFileManager.h>

#include <IPeerManager.h>
#include <Common/LogManager/ILogger.h>
#include <priv/Peer.h>

namespace PM
{
   class Peer;

   struct PendingSocket
   {
      PendingSocket(QTcpSocket* socket) : socket(socket) { this->t.start(); }

      QTcpSocket* socket;
      QTime t;
   };

   class PeerManager : public IPeerManager, Common::Uncopyable
   {
      Q_OBJECT
   public:
      PeerManager(QSharedPointer<FM::IFileManager> fileManager);
      ~PeerManager();

      Common::Hash getID();
      void setNick(const QString& nick);
      QString getNick();

      QList<IPeer*> getPeers();
      IPeer* getPeer(const Common::Hash& ID);
      Peer* getPeer_(const Common::Hash& ID);

      void updatePeer(const Common::Hash& ID, const QHostAddress& IP, quint16 port, const QString& nick, const quint64& sharingAmount);
      void newConnection(QTcpSocket* tcpSocket);

      void onGetChunk(Common::Hash hash, int offset, QSharedPointer<Socket> socket);

   private slots:
      void dataReceived(QTcpSocket* tcpSocket = 0);
      void disconnected(QTcpSocket* tcpSocket = 0);
      void checkIdlePendingSockets();

   private:
      void removeFromPending(QTcpSocket* socket);

      QSharedPointer<FM::IFileManager> fileManager;

      Common::Hash ID;
      QString nick;
      QList<Peer*> peers;

      QTimer timer; ///< Used to check periodically if some pending sockets have timeouted.
      QList<PendingSocket> pendingSockets;
   };
}
#endif
