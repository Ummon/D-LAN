#include <priv/GetHashesResult.h>

#include <QSharedPointer>
#include <QList>
#include <QMutexLocker>
#include <QMetaType>

#include <priv/Cache/File.h>
#include <priv/Cache/Chunk.h>
#include <priv/Log.h>

using namespace FM;

GetHashesResult::GetHashesResult(const Protos::Common::Entry& fileEntry, Cache& cache, FileUpdater& fileUpdater)
   : fileEntry(fileEntry), file(0), cache(cache), fileUpdater(fileUpdater), nbHash(0), lastHashNumSent(-1)
{
   qRegisterMetaType<Common::Hash>("Common::Hash");
}

GetHashesResult::~GetHashesResult()
{
   disconnect(&this->cache, SIGNAL(chunkHashKnown(QSharedPointer<Chunk>)), this, SLOT(chunkHashKnown(QSharedPointer<Chunk>)));
}

/**
  * Called from the main thread.
  */
Protos::Core::GetHashesResult GetHashesResult::start()
{
   Protos::Core::GetHashesResult result;

   this->file = this->cache.getFile(this->fileEntry);
   QList< QSharedPointer<Chunk> > chunks = this->cache.getChunks(this->fileEntry);

   if (chunks.isEmpty())
   {
      result.set_status(Protos::Core::GetHashesResult_Status_DONT_HAVE);
      return result;
   }

   connect(&this->cache, SIGNAL(chunkHashKnown(QSharedPointer<Chunk>)), this, SLOT(chunkHashKnown(QSharedPointer<Chunk>)), Qt::DirectConnection);
   this->nbHash = chunks.size();

   result.set_nb_hash(this->nbHash);
   for (QListIterator< QSharedPointer<Chunk> > i(chunks); i.hasNext();)
   {
      QSharedPointer<Chunk> chunk(i.next());
      if (chunk->hasHash())
      {
         this->sendNextHash(chunk);
      }
      else // If only one hash is missing we tell the FileUpdater to compute the remaining ones.
      {
         this->fileUpdater.prioritizeAFileToHash(this->file);
         break;
      }
   }

   result.set_status(Protos::Core::GetHashesResult_Status_OK);
   return result;
}

/**
  * Called from UploadManager thread.
  */
void GetHashesResult::chunkHashKnown(QSharedPointer<Chunk> chunk)
{
   if (chunk->isOwnedBy(this->file))
   {
      this->sendNextHash(chunk);
   }
}

void GetHashesResult::sendNextHash(QSharedPointer<Chunk> chunk)
{
   QMutexLocker lock(&this->mutex);

   if (chunk->getNum() == this->lastHashNumSent)
      return;

   this->lastHashNumSent = chunk->getNum();

   if (!--this->nbHash)
      disconnect(&this->cache, SIGNAL(chunkHashKnown(QSharedPointer<Chunk>)), this, SLOT(chunkHashKnown(QSharedPointer<Chunk>)));

   emit nextHash(chunk->getHash());
}
