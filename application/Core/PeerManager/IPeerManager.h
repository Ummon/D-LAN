#ifndef PEERMANAGER_IPEERMANAGER_H
#define PEERMANAGER_IPEERMANAGER_H

#include <QString>
#include <QtNetwork>

#include <Common/Hash.h>
#include <Protos/common.pb.h>

namespace PM
{
   class IPeer;
   class ISocket;

   class IPeerManager : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IPeerManager() {}

      /**
        * Return our ID. This ID is taken from the settings in the home folder of the current user.
        * If there is no ID in the settings, this one is randomly generated and saved in the settings.
        */
      virtual Common::Hash getID() = 0;

      /**
        * As the ID, the nick is saved in the file settings in the home folder of the current user.
        */
      virtual void setNick(const QString& nick) = 0;
      virtual QString getNick() = 0;

      virtual QList<IPeer*> getPeers() = 0;

      /**
        * Return the IPeer* coresponding to ID.
        * Return 0 if the peer doesn't exist.
        */
      virtual IPeer* getPeer(const Common::Hash& ID) = 0;

      /**
        * The method must be call frequently to tell that a peer (ID) is still alive.
        */
      virtual void updatePeer(const Common::Hash& ID, const QHostAddress& IP, quint16 port, const QString& nick, const quint64& sharingAmount) = 0;

      /**
        * @param socket PeerManager will care about deleting the socket.
        */
      virtual void newConnection(QTcpSocket* tcpSocket) = 0;

   signals:
      /**
        * When a remote peer want a chunk, this signal is emitted.
        */
      void getChunk(Common::Hash hash, int offset, ISocket* socket);
   };
}
#endif
