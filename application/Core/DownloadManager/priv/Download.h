#ifndef DOWNLOADMANAGER_DOWNLOAD_H
#define DOWNLOADMANAGER_DOWNLOAD_H

#include <QSharedPointer>
#include <QTimer>
#include <QFlags>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IPeer.h>

#include <Protos/common.pb.h>
#include <Protos/queue.pb.h>

#include <IDownload.h>

namespace PM { class IPeer; }

namespace DM
{
   class Download : public QObject, public IDownload
   {
      Q_OBJECT
      static quint64 currentID;

   protected:
      Download(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry);

   public:
      virtual ~Download();

      virtual void populateEntry(Protos::Queue::Queue_Entry* entry) const;

      quint64 getID() const;
      Status getStatus() const;
      bool isStatusErroneous() const;
      virtual int getProgress() const;
      Common::Hash getPeerSourceID() const;
      const Protos::Common::Entry& getEntry();
      void remove();

      bool hasAValidPeer();

   signals:
      void deleted(Download*);

   protected slots:
      virtual void retrievePeer();

   protected:
      /**
        * This method permits to change the behaviour by a subclass when Download change the status.
        */
      virtual void setStatus(Status newStatus);

      const quint64 ID;

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      Common::Hash peerSourceID;
      PM::IPeer* peerSource;
      Protos::Common::Entry entry;

      Status status;
   };
}
#endif
