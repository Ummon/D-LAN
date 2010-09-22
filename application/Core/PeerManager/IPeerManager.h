#ifndef PEERMANAGER_IPEERMANAGER_H
#define PEERMANAGER_IPEERMANAGER_H

#include <QString>
#include <QtNetwork>

#include <Common/Hash.h>
#include <Protos/common.pb.h>

namespace PM
{
   class IPeer;
   class IPeerManager : public QObject
   {
   public:
      virtual ~IPeerManager() {}
      virtual Common::Hash getID() = 0;
      virtual void setNick(const QString& nick) = 0;
      virtual QString getNick() = 0;

      virtual QList<IPeer*> getPeers() = 0;
      virtual IPeer* getPeer(const Common::Hash& ID) = 0;

      /**
        * The method must be call frequently to tell that a peer (ID) is still alive.
        */
      virtual void updatePeer(const Common::Hash& ID, const QHostAddress& IP, quint16 port, const QString& nick, const quint64& sharingAmount) = 0;

      /**
        * @param socket PeerManager will care about deleting the socket.
        */
      virtual void newConnection(QTcpSocket* tcpSocket) = 0;
   };
}
#endif
