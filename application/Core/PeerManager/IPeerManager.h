#ifndef PEERMANAGER_IPEERMANAGER_H
#define PEERMANAGER_IPEERMANAGER_H

#include <QString>
#include <QAbstractSocket>

#include <Common/Hash.h>
#include <Protos/common.pb.h>

namespace PeerManager
{
   class IPeer;
   class IPeerManager
   {
   public:
      virtual ~IPeerManager() {}

      virtual void setNick(const QString& nick_) = 0;
      virtual QString* getNick() = 0;
      /*
      void newPeerConnection(const QAbstractSocket& socket);
      void updatePeer(const Common::Hash& ID, quint32 IP, const QString& nick, const quint64& amount);
      QList<IPeer*> getPeers();
      IPeer* getPeer(const quint32& ID);*/
      virtual Common::Hash* getMyId() = 0;

  /* signals:
      void newUploadRequest(const Common::Hash& hash, const IPeer& peer);*/
   };
}
#endif
