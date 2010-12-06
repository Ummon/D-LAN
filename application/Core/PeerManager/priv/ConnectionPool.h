#ifndef PEERMANAGER_CONNECTIONPOOL_H
#define PEERMANAGER_CONNECTIONPOOL_H

#include <QtNetwork>
#include <QList>
#include <QDateTime>
#include <QSharedPointer>

#include <Common/Uncopyable.h>

#include <Core/FileManager/IFileManager.h>

#include <priv/Socket.h>

namespace PM
{
   class Socket;
   class PeerManager;

   class ConnectionPool : public QObject, Common::Uncopyable
   {
      Q_OBJECT

   public:
      ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager, const Common::Hash& peerID);
      ~ConnectionPool();

      void setIP(const QHostAddress& IP, quint16 port);
      void newConnexion(QTcpSocket* socket);

      QSharedPointer<Socket> getASocket();
      void closeAllSocket();

   private slots:
      void socketGetIdle(Socket* socket);
      void socketClosed(Socket* socket);
      void socketGetChunk(const Common::Hash& hash, int offset, Socket* socket);

   private:
      QSharedPointer<Socket> addNewSocket(QSharedPointer<Socket> socket);

      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;

      QList< QSharedPointer<Socket> > sockets;
      QHostAddress peerIP;
      quint16 port;
      const Common::Hash peerID;
   };
}

#endif
