#ifndef DOWNLOADMANAGER_DOWNLOAD_H
#define DOWNLOADMANAGER_DOWNLOAD_H

#include <QSharedPointer>
#include <QTimer>
#include <QFlags>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IPeer.h>

#include <Protos/common.pb.h>

#include <IDownload.h>

namespace PM { class IPeer; }

namespace DM
{
   class Download : public QObject, public IDownload
   {
      Q_OBJECT
   protected:
      Download(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry);

   public:
      virtual ~Download() {}

      int getId() const;
      Status getStatus() const;
      virtual int getProgress() const;
      Common::Hash getPeerSourceID();
      Protos::Common::Entry getEntry();
      void remove();

      bool hasAValidPeer();

   protected slots:
      virtual void retrievePeer();

   protected:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      Common::Hash peerSourceID;
      PM::IPeer* peerSource;
      Protos::Common::Entry entry;

      Status status;

      QTimer timer; // Used to periodically try to retrieve the peerSource;
   };
}
#endif
