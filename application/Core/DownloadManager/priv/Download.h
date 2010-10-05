#ifndef DOWNLOADMANAGER_DOWNLOAD_H
#define DOWNLOADMANAGER_DOWNLOAD_H

#include <QSharedPointer>
#include <QTimer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IPeer.h>

#include <Protos/common.pb.h>

#include <IDownload.h>

namespace PM { class IPeer; }

namespace DM
{
   class Download : public IDownload
   {
      Q_OBJECT
   public:
      Download(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry);
      virtual ~Download() {}

      int getId();
      Status getStatus();
      char getProgress();
      Common::Hash getPeerSourceID();
      Protos::Common::Entry getEntry();
      void remove();

      bool hasAValidPeer();
      void retreiveHashes();

   private:
      void retrievePeer();

   private slots:
      void result(const Protos::Core::GetHashesResult& result);
      void nextHash(const Common::Hash& hash);

   protected:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      Common::Hash peerSourceID;
      PM::IPeer* peerSource;
      Protos::Common::Entry entry;

      Status status;

      QSharedPointer<PM::IGetHashesResult> getHashesResult;
      int nbHashes;
   };
}
#endif
