#ifndef PEERMANAGER_IPEER_H
#define PEERMANAGER_IPEER_H

#include <QObject>

#include <Protos/common.pb.h>
#include <QHostAddress>

#include <Common/Hashes.h>

namespace PM
{
   class IGetEntries;
   class IPeer : public QObject
   {
   Q_OBJECT
   public:
      virtual ~IPeer() {}

      virtual bool isAlive() = 0;
      virtual bool send(const QByteArray& data) = 0;
      virtual Common::Hashes* getHashes(const Protos::Common::FileEntry& file) = 0;
      virtual IGetEntries* getEntries(const Protos::Common::DirEntry& dir) = 0;
      virtual QHostAddress getIp() = 0;

   signals:
      virtual void receive(QByteArray& data) = 0;

   public slots:
      virtual void connected() = 0;
      virtual void gotData() = 0;
   };
}
#endif
