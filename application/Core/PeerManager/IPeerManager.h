#ifndef PEERMANAGER_IPEERMANAGER_H
#define PEERMANAGER_IPEERMANAGER_H

#include <QString>
#include <QAbstractSocket>

#include <Common/Hash.h>

namespace PeerManager
{
   class IPeer;
   class IPeerManager
   {
   public:
      virtual ~IPeerManager() {}

      void setNick(const QString& nick);
      QString getNick();
      void newPeerConnection(const QAbstractSocket& socket);
      void updatePeer(const Common::Hash& ID, quint32 IP, const QString& nick, const quint64& amount);
      QList<IPeer*> getPeers();
      IPeer* getPeer(const quint32& ID);

   signals:
      void newUploadRequest(const Common::Hash& hash, const IPeer& peer);
   };
}
#endif
