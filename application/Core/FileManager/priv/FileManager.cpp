/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <priv/FileManager.h>
using namespace FM;

#include <functional>

#include <QSharedPointer>
#include <QStringList>
#include <QStringBuilder>
#include <QList>
#include <QVector>
#include <QDir>
#include <QMutableListIterator>

#include <google/protobuf/text_format.h>

#include <Common/KnownExtensions.h>
#include <Common/PersistentData.h>
#include <Common/Settings.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/SharedEntry.h>
#include <Common/StringUtils.h>

#include <Protos/gui_settings.pb.h>

#include <Exceptions.h>
#include <priv/Global.h>
#include <priv/Constants.h>
#include <priv/GetHashesResult.h>
#include <priv/GetEntriesResult.h>
#include <priv/Cache/Entry.h>
#include <priv/Cache/File.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedEntry.h>
#include <priv/Cache/Chunk.h>

LOG_INIT_CPP(FileManager)

FileManager::FileManager(QSharedPointer<HC::IHashCache> hashCache) :
   fileUpdater(this),
   cache(hashCache),
   cacheLoading(true)
{
   Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;

   connect(&this->cache, &Cache::entryAdded, this, &FileManager::entryAdded, Qt::DirectConnection);
   connect(&this->cache, &Cache::entryRemoved, this, &FileManager::entryRemoved, Qt::DirectConnection);
   connect(&this->cache, &Cache::entryRenamed, this, &FileManager::entryRenamed, Qt::DirectConnection);
   connect(&this->cache, &Cache::chunkHashKnown, this, &FileManager::chunkHashKnown, Qt::DirectConnection);
   connect(&this->cache, &Cache::chunkRemoved, this, &FileManager::chunkRemoved, Qt::DirectConnection);

   connect(&this->cache, &Cache::newSharedEntry, this, &FileManager::newSharedEntry, Qt::DirectConnection);
   connect(&this->cache, &Cache::sharedEntryRemoved, this, &FileManager::sharedEntryRemoved, Qt::DirectConnection);

   connect(&this->fileUpdater, &FileUpdater::fileCacheLoaded, this, &FileManager::fileCacheLoadingComplete, Qt::QueuedConnection);
   connect(&this->fileUpdater, &FileUpdater::deleteSharedEntry, this, &FileManager::deleteSharedEntry, Qt::QueuedConnection); // If the 'FileUpdater' wants to delete a shared directory.

   // Give stored shared entries to the cache:
   SETTINGS.get<Protos::Common::SharedEntry>("shared_entry");

   this->fileUpdater.addRoot();

   // TODO: call addRoot for each shared entry in settings.
   this->fileUpdater.start();
}

FileManager::~FileManager()
{
   L_DEBU("~FileManager: Stopping the file updater . . .");
   this->fileUpdater.stop();
   this->cache.disconnect(this);
   L_DEBU("FileManager deleted");
}

/**
  * @exception ItemsNotFoundException
  */
void FileManager::setSharedPaths(const QStringList& paths)
{
   this->cache.setSharedPaths(paths);
}

/**
  * @exception ItemsNotFoundException
  */
QPair<Common::SharedEntry, QString> FileManager::addASharedPath(const QString& absolutePath)
{
   return this->cache.addASharedPath(absolutePath);
}

QList<Common::SharedEntry> FileManager::getSharedEntries() const
{
   return this->cache.getSharedEntries();
}

QString FileManager::getSharedEntry(const Common::Hash& ID) const
{
   SharedEntry* entry = this->cache.getSharedEntry(ID);
   if (entry)
      return entry->getPath().getPath();
   else
      return QString();
}

QSharedPointer<IChunk> FileManager::getChunk(const Common::Hash& hash) const
{
   return this->chunks.value(hash);
}

QList<QSharedPointer<IChunk>> FileManager::getAllChunks(const Protos::Common::Entry& localEntry, const Common::Hashes& hashes) const
{
   for (QListIterator<Common::Hash> h(hashes); h.hasNext();)
   {
      // Chunks from different files, usually one chunk.
      const QList<QSharedPointer<Chunk>>& chunks = this->chunks.values(h.next());

      for (QListIterator<QSharedPointer<Chunk>> i(chunks); i.hasNext();)
      {
         QSharedPointer<Chunk> chunk = i.next();
         if (chunk->matchesEntry(localEntry)) // The name, the path and the size of the file are the same?
         {
            // We verify that all hashes of all chunks match the given hashes. If it's not the case, the files are not the same.
            QVector<QSharedPointer<Chunk>> allChunks = chunk->getOtherChunks();
            if (allChunks.size() != hashes.size())
               return QList<QSharedPointer<IChunk>>();

            QList<QSharedPointer<IChunk>> ret;
            for (int j = 0; j < allChunks.size(); j++)
            {
               if (allChunks[j]->getHash() != hashes[j]) // Only one hashes doesn't match -> all the file doesn't match.
                  return QList<QSharedPointer<IChunk>>();
               ret << allChunks[j];
            }
            return ret;
         }
      }
   }

   return QList<QSharedPointer<IChunk>>();
}

QList<QSharedPointer<IChunk>> FileManager::newFile(Protos::Common::Entry& entry)
{
   return this->cache.newFile(entry);
}

void FileManager::newDirectory(Protos::Common::Entry& entry)
{
   this->cache.newDirectory(entry);
}

QSharedPointer<IGetHashesResult> FileManager::getHashes(const Protos::Common::Entry& file)
{
   return QSharedPointer<IGetHashesResult>(new GetHashesResult(file, this->cache, this->fileUpdater));
}

QSharedPointer<IGetEntriesResult> FileManager::getScannedEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry)
{
   return QSharedPointer<IGetEntriesResult>(new GetEntriesResult(this->cache.getDirectory(dir), maxNbHashesPerEntry));
}

Protos::Common::Entries FileManager::getEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry)
{
   return this->cache.getProtoEntries(dir, maxNbHashesPerEntry);
}

Protos::Common::Entries FileManager::getEntries()
{
   return this->cache.getProtoSharedEntries();
}

QList<Protos::Common::FindResult> FileManager::find(const QString& words, const QList<QString>& extensions, qint64 minFileSize, qint64 maxFileSize, Protos::Common::FindPattern_Category category, int maxNbResult, int maxSize)
{
   bool filterBySizeOn = minFileSize > 0 || maxFileSize != std::numeric_limits<qint64>::max();
   bool filterByExtensionsOn = !extensions.isEmpty();
   bool filterByCategoryOn = category != Protos::Common::FindPattern::FILE_DIR;
   bool filterOn = filterBySizeOn || filterByExtensionsOn || filterByCategoryOn;

   QList<NodeResult<Entry*>> result;

   if (!words.isEmpty())
   {
      result = !filterOn
         ? this->wordIndex.search(Common::StringUtils::splitInWords(words), maxNbResult)
         : this->wordIndex.search(
            Common::StringUtils::splitInWords(words), maxNbResult, [&](const Entry* entry) {
               return (!filterBySizeOn || entry->getSize() >= minFileSize && entry->getSize() <= maxFileSize) &&
                      (!filterByExtensionsOn || extensions.contains(entry->getExtension())) &&
                      (!filterByCategoryOn || (category == Protos::Common::FindPattern::FILE && dynamic_cast<const File*>(entry) || category == Protos::Common::FindPattern::DIR && dynamic_cast<const Directory*>(entry)));
            }
         );
   }
   else if (filterBySizeOn || filterByExtensionsOn)
   {
      QList<Entry*> intermediateResult;

      if (!extensions.isEmpty())
      {
         if (filterBySizeOn || filterByCategoryOn)
            intermediateResult =
                  this->extensionIndex.search(
                     extensions,
                     maxNbResult,
                     [&](const Entry* entry)
                     {
                        return (!filterBySizeOn || entry->getSize() >= minFileSize && entry->getSize() <= maxFileSize) && (!filterByCategoryOn || (category == Protos::Common::FindPattern::FILE && dynamic_cast<const File*>(entry) || category == Protos::Common::FindPattern::DIR && dynamic_cast<const Directory*>(entry)));
                     }
                  );
         else
            intermediateResult = this->extensionIndex.search(extensions, maxNbResult);
      }
      else
      {
         if (filterByCategoryOn)
            intermediateResult =
               this->sizeIndex.search(
                     minFileSize,
                     maxFileSize,
                     maxNbResult,
                     [&](const Entry* entry)
                     {
                        return category == Protos::Common::FindPattern::FILE && dynamic_cast<const File*>(entry) || category == Protos::Common::FindPattern::DIR && dynamic_cast<const Directory*>(entry);
                     }
               );
         else
            intermediateResult = this->sizeIndex.search(minFileSize, maxFileSize, maxNbResult);
      }

      for (QListIterator<Entry*> i(intermediateResult); i.hasNext();)
         result << NodeResult<Entry*>(i.next());
   }

   QList<Protos::Common::FindResult> findResults;
   findResults << Protos::Common::FindResult();
   findResults.last().set_tag(std::numeric_limits<quint64>::max()); // Worst case to compute the size (int fields have a variable size).

   const int EMPTY_FIND_RESULT_SIZE = findResults.last().ByteSizeLong();
   int findResultCurrentSize = EMPTY_FIND_RESULT_SIZE; // [Byte].

   for (QListIterator<NodeResult<Entry*>> i(result); i.hasNext();)
   {
      const NodeResult<Entry*>& entry = i.next();
      Protos::Common::FindResult::EntryLevel* entryLevel = findResults.last().add_entry();
      entryLevel->set_level(entry.level);

      File* file = dynamic_cast<File*>(entry.value);
      if (file)
         file->populateEntry(entryLevel->mutable_entry(), true, NB_MAX_HASHES_PER_ENTRY_SEARCH);
      else
         entry.value->populateEntry(entryLevel->mutable_entry(), true);

      // We wouldn't use 'findResults.last().ByteSize()' because is too slow. Instead we call 'ByteSize()' for each entry and sum it.
      const int entryByteSize = entryLevel->ByteSizeLong() + 8; // Each entry take a bit of memory overhead . . . (Value found in an empiric way . . .).
      findResultCurrentSize += entryByteSize;

      if (findResultCurrentSize > maxSize)
      {
         google::protobuf::RepeatedPtrField<Protos::Common::FindResult::EntryLevel>* entries = findResults.last().mutable_entry();
         findResults << Protos::Common::FindResult();
         if (entries->size() > 0)
         {
            findResults.last().add_entry()->CopyFrom(entries->Get(entries->size()-1));
            entries->RemoveLast();
         }
         findResultCurrentSize = EMPTY_FIND_RESULT_SIZE + entryByteSize;
      }
   }

   if (findResults.last().entry_size() == 0)
      findResults.removeLast();

   return findResults;
}

QBitArray FileManager::haveChunks(const QList<Common::Hash>& hashes)
{
   QBitArray result(hashes.size()); // All bits to 0 by default.
   bool ownsAtLeastOneChunk = false;
   for (int i = 0; i < hashes.size(); i++)
      if (this->chunks.contains(hashes[i]))
      {
         result.setBit(i, true);
         ownsAtLeastOneChunk = true;
      }

   if (!ownsAtLeastOneChunk)
      return QBitArray();

   return result;
}

quint64 FileManager::getAmount()
{
   return this->cache.getAmount();
}

FileManager::CacheStatus FileManager::getCacheStatus() const
{
   if (this->fileUpdater.isScanning())
      return SCANNING_IN_PROGRESS;

   if (this->fileUpdater.isHashing())
      return HASHING_IN_PROGRESS;

   return UP_TO_DATE;
}

int FileManager::getProgress() const
{
   return this->fileUpdater.getProgress();
}

void FileManager::dumpWordIndex() const
{
   L_WARN(this->wordIndex.toStringLog());
}

/**
  * Incomplete, only the first hash is compared for the moment.
  */
void FileManager::printSimilarFiles() const
{
   QString result("Similar files:\n");

   QSet<Common::Hash> knownHashes;
   foreach (Common::SharedEntry sharedEntry, this->cache.getSharedEntries())
   {
      Entry* entry = this->cache.getSharedEntry(sharedEntry.ID)->getRootEntry();

      FileIterator i(entry);

      while (File* file = i.next())
      {
         const QVector<QSharedPointer<Chunk>>& chunks = file->getChunks();
         if (!chunks.isEmpty())
         {
            const Common::Hash& hash = chunks[0]->getHash();
            if (!hash.isNull() && !knownHashes.contains(hash))
            {
               knownHashes.insert(hash);
               const QList<QSharedPointer<Chunk>>& similarChunks = this->chunks.values(hash);
               if (similarChunks.size() > 1)
               {
                  foreach (QSharedPointer<Chunk> similarChunk, similarChunks)
                     result.append(similarChunk->getFilePath()).append("\n");
                  result.append("------\n");
               }
            }
         }
      }
   }

   L_WARN(result);
}

Directory* FileManager::getFittestDirectory(const QString& path)
{
   return this->cache.getFittestDirectory(path);
}

/**
  * Used to retrieve a file or a directory by the fileUpdater when a filesystem event occurs.
  */
Entry* FileManager::getEntry(const QString& path) const
{
   return this->cache.getEntry(path);
}

SharedEntry* FileManager::getSharedEntry(const QString& path) const
{
   return this->cache.getSharedEntry(path);
}

void FileManager::newSharedEntry(SharedEntry* sharedEntry)
{
   this->fileUpdater.addRoot(sharedEntry);
}

void FileManager::sharedEntryRemoved(SharedEntry* sharedEntry, Directory* dir)
{
   this->fileUpdater.rmRoot(sharedEntry, dir);
   //this->forcePersistCacheToFile();
}

void FileManager::deleteSharedEntry(SharedEntry* sharedEntry)
{
   this->cache.removeSharedEntry(sharedEntry);
}

void FileManager::entryAdded(Entry* entry)
{
   if (entry->getName().isEmpty() || Global::isFileUnfinished(entry->getName()))
      return;

   L_DEBU(QString("Adding entry '%1' to the index . . .").arg(entry->getName()));
   this->wordIndex.addItem(Common::StringUtils::splitInWords(entry->getNameWithoutExtension()), entry);
   this->extensionIndex.addItem(entry->getExtension(), entry);
   if (!this->cacheLoading)
      this->sizeIndex.addItem(entry);
   L_DEBU("Entry added to the index");
}

void FileManager::entryRemoved(Entry* entry)
{
   if (entry->getName().isEmpty())
      return;

   L_DEBU(QString("Removing entry '%1' from the index . . .").arg(entry->getName()));
   if (!this->wordIndex.rmItem(Common::StringUtils::splitInWords(entry->getName()), entry))
      L_DEBU(QString("The entry '%1' hasn't been found in the index!").arg(entry->getName()));
   this->extensionIndex.rmItem(entry->getExtension(), entry);
   this->sizeIndex.rmItem(entry);
   L_DEBU("Entry removed from the index");
}

void FileManager::entryRenamed(Entry* entry, const QString& oldName)
{
   L_DEBU(QString("Renaming entry '%1' to '%2' in the index . . .").arg(entry->getName()).arg(oldName));
   this->wordIndex.renameItem(Common::StringUtils::splitInWords(oldName), Common::StringUtils::splitInWords(entry->getName()), entry);
   this->extensionIndex.changeItem(Common::KnownExtensions::getExtension(oldName), entry->getExtension(), entry);
   L_DEBU("Entry renamed in the index");
}

void FileManager::entryResizing(Entry* entry)
{
   this->sizeIndex.rmItem(entry);
}

void FileManager::entryResized(Entry* entry, qint64 oldSize)
{
   this->sizeIndex.addItem(entry);
}

void FileManager::chunkHashKnown(const QSharedPointer<Chunk>& chunk)
{
   L_DEBU(QString("Adding chunk '%1' to the index . . .").arg(chunk->getHash().toStr()));
   this->chunks.add(chunk);
   L_DEBU("Chunk added to the index");
   this->setCacheChanged();
}

void FileManager::chunkRemoved(const QSharedPointer<Chunk>& chunk)
{
   L_DEBU(QString("Removing chunk '%1' from the index . . .").arg(chunk->getHash().toStr()));
   this->chunks.rm(chunk);
   L_DEBU("Chunk removed from the index");
   this->setCacheChanged();
}

/**
  * Load the cache from a file. Called at start, by the constructor.
  * It will give the file cache to the fileUpdater and ask it
  * to load the cache.
  * It will also start the timer to persist the cache.
  */
//void FileManager::loadCacheFromFile()
//{
   // TODO : move old shared dir from cache file to settings.

   // This hashes will be unallocated by the fileUpdater.
//   Protos::FileCache::Hashes* savedCache = new Protos::FileCache::Hashes();

//   try
//   {
//      Common::PersistentData::getValue(Common::Constants::FILE_CACHE, *savedCache, Common::Global::DataFolderType::LOCAL);
//      if (static_cast<int>(savedCache->version()) != FILE_CACHE_VERSION)
//      {
//         L_ERRO(QString("The version (%1) of the file cache \"%2\" doesn't match the current version (%3)").arg(savedCache->version()).arg(Common::Constants::FILE_CACHE).arg(FILE_CACHE_VERSION));
//         Common::PersistentData::rmValue(Common::Constants::FILE_CACHE, Common::Global::DataFolderType::LOCAL);
//         delete savedCache;
//         return;
//      }

//      // Scan the shared directories and try to match the files against the saved cache.
//      try
//      {
//         this->cache.createSharedDirs(*savedCache);
//      }
//      catch (ItemsNotFoundException& e)
//      {
//         foreach (QString path, e.paths)
//            L_WARN(QString("During the file cache loading, this directory hasn't been found: %1").arg(path));
//      }
//   }
//   catch (Common::UnknownValueException& e)
//   {
//      L_WARN(QString("The persisted file cache cannot be retrieved (the file doesn't exist): %1").arg(Common::Constants::FILE_CACHE));
//   }
//   catch (...)
//   {
//      L_WARN(QString("The persisted file cache cannot be retrieved (Unknown exception): %1").arg(Common::Constants::FILE_CACHE));
//   }

//   this->fileUpdater.setFileCache(savedCache);
//}

/**
  * Save the cache to a file.
  * Restart the timer at the end of the operation.
  * Called by the fileUpdater when it needs to persist the cache.
  */
/*
void FileManager::persistCacheToFile()
{
   QMutexLocker locker(&this->mutexPersistCache);

   QMutexLocker lockerCacheChanged(&this->mutexCacheChanged);
   if (this->cacheChanged && !this->cacheLoading)
   {
      lockerCacheChanged.unlock();

      L_DEBU("Persisting cache . . .");

      Protos::FileCache::Hashes hashes;
      this->cache.populateHashes(hashes);

      try
      {
         Common::PersistentData::setValue(Common::Constants::FILE_CACHE, hashes, Common::Global::DataFolderType::LOCAL);
      }
      catch (Common::PersistentDataIOException& err)
      {
         L_ERRO(err.message);
      }

      lockerCacheChanged.relock();
      this->cacheChanged = false;

      L_DEBU("Persisting cache finished");
   }

   this->timerPersistCache.start();
}
*/

/*
void FileManager::forcePersistCacheToFile()
{
   this->mutexCacheChanged.lock();
   this->cacheChanged = true;
   this->mutexCacheChanged.unlock();

   QMutexLocker locker(&this->mutexPersistCache);
   this->persistCacheToFile();
}
*/

/**
  * @warning Can be called from different threads like a 'Downloader' or the 'FileUpdater'.
  */
void FileManager::setCacheChanged()
{
   QMutexLocker locker(&this->mutexCacheChanged);
   this->cacheChanged = true;
}

void FileManager::fileCacheLoadingComplete()
{
   connect(&this->cache, &Cache::entryResizing, this, &FileManager::entryResizing, Qt::DirectConnection);
   connect(&this->cache, &Cache::entryResized, this, &FileManager::entryResized, Qt::DirectConnection);

   this->cache.forall(
      [&](Entry* entry)
      {
         this->sizeIndex.addItem(entry);
      }
   );

   this->cacheLoading = false;

   emit fileCacheLoaded();
}
