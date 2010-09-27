#ifndef PEERMANAGER_CONNECTIONPOOL_H
#define PEERMANAGER_CONNECTIONPOOL_H

#include <QtNetwork>
#include <QList>
#include <QDateTime>
#include <QTimer>

#include <Core/FileManager/IFileManager.h>

#include <priv/Socket.h>

namespace PM
{
   class Socket;
   class PeerManager;

   class ConnectionPool : public QObject
   {
      Q_OBJECT

   public:
      ConnectionPool(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager);

      void setIP(const QHostAddress& IP, quint16 port);
      void newConnexion(QTcpSocket* socket);

      Socket* getASocket();
      void closeAllSocket();

   private slots:
      void socketGetIdle(Socket* socket);

   private:
      Socket* addNewSocket(Socket* socket);

      PeerManager* peerManager;
      QSharedPointer<FM::IFileManager> fileManager;

      QTimer timer;
      QList<Socket*> sockets;
      QHostAddress peerIP;
      quint16 port;
   };
}

#endif
