#include <priv/FileDownload.h>
using namespace DM;

FileDownload::FileDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager>, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : Download(fileManager, peerManager, peerSourceID, entry)
{

}

void FileDownload::retreiveHashes()
{
   if (!this->hasAValidPeer())
      return;

   this->status |= ASKING_FOR_HASHES;

   this->getHashesResult = this->peerSource->getHashes(this->entry);
   connect(this->getHashesResult.data(), SIGNAL(result(const Protos::Core::GetHashesResult&)), this, SLOT(result(const Protos::Core::GetHashesResult&)));
   connect(this->getHashesResult.data(), SIGNAL(nextHash(const Common::Hash&)), this, SLOT(nextHash(const Common::Hash&)));
   this->getHashesResult->start();
}

void FileDownload::result(const Protos::Core::GetHashesResult& result)
{
   if (result.status() == Protos::Core::GetHashesResult_Status_OK)
   {
      this->nbHashes = result.nb_hash();
   }
   else
   {
      this->status &= !ASKING_FOR_HASHES;
      this->status |= ENTRY_NOT_FOUND;
      this->getHashesResult.clear();
   }
}

void FileDownload::nextHash(const Common::Hash& hash)
{

}
