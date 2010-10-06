#include <priv/DownloadManager.h>
using namespace DM;

#include <Common/Settings.h>

#include <priv/FileDownload.h>
#include <priv/DirDownload.h>
#include <priv/ChunkDownloader.h>
#include <priv/Constants.h>

DownloadManager::DownloadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager)
   : fileManager(fileManager), peerManager(peerManager)
{
   for (int i = 0; i < SETTINGS.getUInt32("number_of_downloader"); i++)
      this->chunkDownloaders << new ChunkDownloader();
}

/**
  * Insert a new download at the end.
  */
void DownloadManager::addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry)
{
   QMutableLinkedListIterator<Download*> i(this->downloads);
   i.toBack();
   this->addDownload(peerSource, entry, i);
}

/**
  * Insert a new download at the given position.
  */
void DownloadManager::addDownload(Common::Hash peerSource, const Protos::Common::Entry& entry, QMutableLinkedListIterator<Download*> iterator)
{
   switch (entry.type())
   {
   case Protos::Common::Entry_Type_DIR :
      {
         DirDownload* dirDownload = new DirDownload(this->fileManager, this->peerManager, peerSource, entry);
         connect(dirDownload, SIGNAL(newEntries(Protos::Core::GetEntriesResult)), this, SLOT(newEntries(Protos::Core::GetEntriesResult)));
         iterator.insert(dirDownload);
         dirDownload->retrieveEntries();
      }
      break;

   case Protos::Common::Entry_Type_FILE :
      {
         FileDownload* fileDownload = new FileDownload(this->fileManager, this->peerManager, peerSource, entry);
         if (fileDownload->hasAValidPeer() && !this->peerIDAskingHashes.contains(fileDownload->getPeerSourceID()))
         {
            this->peerIDAskingHashes.insert(fileDownload->getPeerSourceID());
            fileDownload->retreiveHashes();
         }
         iterator.insert(fileDownload);
      }
      break;
   }
}

QList<IDownload*> DownloadManager::getDownloads()
{
   // TODO
   return QList<IDownload*>();
}

QList< QSharedPointer<IChunkDownload> > DownloadManager::getUnfinishedChunks(int n)
{
   // TODO
   return QList< QSharedPointer<IChunkDownload> >();
}

void DownloadManager::newEntries(const Protos::Core::GetEntriesResult& entries)
{
   DirDownload* dirDownload = dynamic_cast<DirDownload*>(this->sender());
   QMutableLinkedListIterator<Download*> i(this->downloads);
   if (!i.findNext(dirDownload))
      return;
   i.remove();

   for (int n = 0; n < entries.entry_size(); n++)
      this->addDownload(dirDownload->getPeerSourceID(), entries.entry(n), i);
}
