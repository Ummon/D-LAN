#ifndef PEERMANAGER_IPEER_H
#define PEERMANAGER_IPEER_H

#include <QObject>

#include <Protos/common.pb.h>

#include <Common/Hashes.h>

namespace PeerManager
{
   class IGetEntries;
   class IPeer : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IPeer() {}

      virtual bool isAlive() = 0;
      virtual void send(const QByteArray& data) = 0;
      virtual Common::Hashes getHashes(const Protos::Common::FileEntry& file) = 0;
      virtual IGetEntries* getEntries(const Protos::Common::DirEntry& dir) = 0;

   signals:
      void receive(QByteArray& data);
   };
}
#endif
