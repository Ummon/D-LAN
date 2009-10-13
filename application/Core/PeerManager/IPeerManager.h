#ifndef PEERMANAGER_IPEERMANAGER_H
#define PEERMANAGER_IPEERMANAGER_H

#include <QString>
#include <QAbstractSocket>

#include <Common/Hash.h>
#include <Protos/common.pb.h>

namespace PeerManager
{
   class IPeer;
   class IPeerManager : public QObject {
       Q_OBJECT

       public:
          virtual ~IPeerManager() {}


      virtual void setNick(const QString& newNick) = 0;
      virtual QString* getNick() = 0;
      virtual void updatePeer(const Common::Hash& peerID, quint32 peerIP, const QString& peerNick, const quint64& peerAmount) = 0;
      /*
      void newPeerConnection(const QAbstractSocket& socket);
      QList<IPeer*> getPeers();
      IPeer* getPeer(const quint32& ID);*/
      virtual Common::Hash getMyId() = 0;

         /* Dues to a limiation of QObject (cannot inherit more than one QObject class), we must have the def of
            cleanUp, used in PeerManager, here. TODO: Find a better solution ? */
            public slots:
                virtual void cleanUp() = 0;

      /* signals:
          void newUploadRequest(const Common::Hash& hash, const IPeer& peer);*/
   };
}
#endif
