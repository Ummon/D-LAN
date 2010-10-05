#include <priv/DownloadManager.h>
using namespace DM;

#include <priv/FileDownload.h>
#include <priv/DirDownload.h>

DownloadManager::DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager)
   : fileManager(fileManager), peerManager(peerManager)
{
}

void DownloadManager::addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry)
{
   switch (entry.type())
   {
   case Protos::Common::Entry_Type_DIR :
      this->addNewDownload(new FileDownload(this->fileManager, this->peerManager, peerSource, entry));
      break;

   case Protos::Common::Entry_Type_FILE :
      this->downloads << new DirDownload(this->fileManager, this->peerManager, peerSource, entry);
      break;
   }
}

void DownloadManager::addNewDownload(FileDownload* download)
{
   this->downloads << download;

   if (download->hasAValidPeer() && !this->peerIDAskingHashes.contains(download->getPeerSourceID()))
   {
      this->peerIDAskingHashes.insert(download->getPeerSourceID());
      download->retreiveHashes();
   }
}
