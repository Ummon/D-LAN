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

      int getId();
      Status getStatus();
      char getProgress();
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

      enum InternalStatus
      {
         QUEUED = 0x1,
         ASKING_FOR_HASHES = 0x2,
         DOWNLOADING = 0x4,
         COMPLETE = 0x8,
         PAUSED = 0x10,
         UNKNOWN_PEER = 0x20,
         ENTRY_NOT_FOUND = 0x40,
         NO_SOURCE = 0x80,
      };
      QFlags<InternalStatus> status;

      QTimer timer;
   };
}
#endif
