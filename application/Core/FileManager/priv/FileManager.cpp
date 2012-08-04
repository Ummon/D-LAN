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

#include <limits>

#include <QSharedPointer>
#include <QStringList>
#include <QStringBuilder>
#include <QList>
#include <QVector>
#include <QDir>
#include <QMutableListIterator>

#include <google/protobuf/text_format.h>

#include <Protos/files_cache.pb.h>

#include <Common/PersistentData.h>
#include <Common/Settings.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/StringUtils.h>
#include <Exceptions.h>
#include <priv/Global.h>
#include <priv/Constants.h>
#include <priv/GetHashesResult.h>
#include <priv/GetEntriesResult.h>
#include <priv/Cache/Entry.h>
#include <priv/Cache/File.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>

LOG_INIT_CPP(FileManager);

FileManager::FileManager() :
   fileUpdater(this),
   cache(),
   mutexPersistCache(QMutex::Recursive),
   cacheLoading(true),
   cacheChanged(false)
{
   Chunk::CHUNK_SIZE = SETTINGS.get<quint32>("chunk_size");

   connect(&this->cache, SIGNAL(entryAdded(Entry*)),     this, SLOT(entryAdded(Entry*)),     Qt::DirectConnection);
   connect(&this->cache, SIGNAL(entryRemoved(Entry*)),   this, SLOT(entryRemoved(Entry*)),   Qt::DirectConnection);
   connect(&this->cache, SIGNAL(entryRenamed(Entry*, QString)),   this, SLOT(entryRenamed(Entry*, QString)),   Qt::DirectConnection);
   connect(&this->cache, SIGNAL(chunkHashKnown(QSharedPointer<Chunk>)), this, SLOT(chunkHashKnown(QSharedPointer<Chunk>)), Qt::DirectConnection);
   connect(&this->cache, SIGNAL(chunkRemoved(QSharedPointer<Chunk>)),   this, SLOT(chunkRemoved(QSharedPointer<Chunk>)),   Qt::DirectConnection);

   connect(&this->cache, SIGNAL(newSharedDirectory(SharedDirectory*)),                 this, SLOT(newSharedDirectory(SharedDirectory*)),                 Qt::DirectConnection);
   connect(&this->cache, SIGNAL(sharedDirectoryRemoved(SharedDirectory*, Directory*)), this, SLOT(sharedDirectoryRemoved(SharedDirectory*, Directory*)), Qt::DirectConnection);

   connect(&this->fileUpdater, SIGNAL(fileCacheLoaded()), this, SLOT(fileCacheLoadingComplete()),  Qt::QueuedConnection);
   connect(&this->fileUpdater, SIGNAL(deleteSharedDir(SharedDirectory*)), this, SLOT(deleteSharedDir(SharedDirectory*)),  Qt::QueuedConnection); // If the 'FileUpdater' wants to delete a shared directory.

   this->timerPersistCache.setInterval(SETTINGS.get<quint32>("save_cache_period"));
   connect(&this->timerPersistCache, SIGNAL(timeout()), this, SLOT(persistCacheToFile()));

   this->loadCacheFromFile();

   this->fileUpdater.start();
}

FileManager::~FileManager()
{
   L_DEBU("~FileManager : Stopping the file updater..");
   this->fileUpdater.stop();
   this->cacheChanged = true;
   this->forcePersistCacheToFile();
   this->timerPersistCache.stop();
   this->cache.disconnect(this);
   L_DEBU("FileManager deleted");
}

/**
  * @exception DirsNotFoundException
  */
void FileManager::setSharedDirs(const QStringList& dirs)
{
   this->cache.setSharedDirs(dirs);
}

/**
  * @exception DirsNotFoundException
  */
QPair<Common::SharedDir, QString> FileManager::addASharedDir(const QString& absoluteDir)
{
   return this->cache.addASharedDir(absoluteDir);
}

QList<Common::SharedDir> FileManager::getSharedDirs() const
{
   return this->cache.getSharedDirs();
}

QString FileManager::getSharedDir(const Common::Hash& ID) const
{
   SharedDirectory* dir = this->cache.getSharedDirectory(ID);
   if (dir)
      return dir->getFullPath();
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

QSharedPointer<IGetEntriesResult> FileManager::getScannedEntries(const Protos::Common::Entry& dir)
{
   return QSharedPointer<IGetEntriesResult>(new GetEntriesResult(this->cache.getDirectory(dir)));
}

Protos::Common::Entries FileManager::getEntries(const Protos::Common::Entry& dir)
{
   return this->cache.getEntries(dir);
}

Protos::Common::Entries FileManager::getEntries()
{
   return this->cache.getSharedEntries();
}

QList<Protos::Common::FindResult> FileManager::find(const QString& words, int maxNbResult, int maxSize)
{
   QList<NodeResult<Entry*>> result = this->wordIndex.search(Common::StringUtils::splitInWords(words), maxNbResult);

   QList<Protos::Common::FindResult> findResults;
   findResults << Protos::Common::FindResult();
   findResults.last().set_tag(std::numeric_limits<quint64>::max()); // Worst case to compute the size (int fields have a variable size).

   const int EMPTY_FIND_RESULT_SIZE = findResults.last().ByteSize();
   int findResultCurrentSize = EMPTY_FIND_RESULT_SIZE; // [Byte].

   for (QListIterator<NodeResult<Entry*>> i(result); i.hasNext();)
   {
      const NodeResult<Entry*>& entry = i.next();
      Protos::Common::FindResult::EntryLevel* entryLevel = findResults.last().add_entry();
      entryLevel->set_level(entry.level);
      entry.value->populateEntry(entryLevel->mutable_entry(), true);

      // We wouldn't use 'findResults.last().ByteSize()' because is too slow. Instead we call 'ByteSize()' for each entry and sum it.
      const int entryByteSize = entryLevel->ByteSize() + 8; // Each entry take a bit of memory overhead... (Value found in an empiric way..).
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
   if (this->cacheLoading)
      return LOADING_CACHE_IN_PROGRSS;

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
   foreach (Common::SharedDir sharedDir, this->cache.getSharedDirs())
   {
      Directory* dir = this->cache.getSharedDirectory(sharedDir.ID);
      DirIterator i(dir, true);
      while (dir = i.next())
      {
         foreach (File* file, dir->getFiles())
         {
            const QVector<QSharedPointer<Chunk>>& chunks = file->getChunks();
            if (!chunks.isEmpty())
            {
               const Common::Hash& hash = chunks[0]->getHash();
               if (!hash.isNull() && !knownHashes.contains(hash))
               {
                  knownHashes.insert(hash);
                  const QList<QSharedPointer<Chunk> >& similarChunks = this->chunks.values(hash);
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
Entry* FileManager::getEntry(const QString& path)
{
   return this->cache.getEntry(path);
}

void FileManager::newSharedDirectory(SharedDirectory* sharedDir)
{
   this->fileUpdater.addRoot(sharedDir);
   this->forcePersistCacheToFile();
}

void FileManager::sharedDirectoryRemoved(SharedDirectory* sharedDir, Directory* dir)
{
   this->fileUpdater.rmRoot(sharedDir, dir);
   this->forcePersistCacheToFile();
}

void FileManager::deleteSharedDir(SharedDirectory* sharedDirectory)
{
   this->cache.removeSharedDir(sharedDirectory);
}

void FileManager::entryAdded(Entry* entry)
{
   if (entry->getName().isEmpty() || Global::isFileUnfinished(entry->getName()))
      return;

   L_DEBU(QString("Adding entry '%1' to the index ..").arg(entry->getName()));
   this->wordIndex.addItem(Common::StringUtils::splitInWords(entry->getName()), entry);
   L_DEBU("Entry added to the index ..");
}

void FileManager::entryRemoved(Entry* entry)
{
   if (entry->getName().isEmpty())
      return;

   L_DEBU(QString("Removing entry '%1' from the index..").arg(entry->getName()));
   this->wordIndex.rmItem(Common::StringUtils::splitInWords(entry->getName()), entry);
   L_DEBU("Entry removed from the index..");
}

void FileManager::entryRenamed(Entry* entry, const QString& oldName)
{
   L_DEBU(QString("Renaming entry '%1' to '%2' in the index..").arg(entry->getName()).arg(oldName));
   this->wordIndex.renameItem(Common::StringUtils::splitInWords(oldName), Common::StringUtils::splitInWords(entry->getName()), entry);
   L_DEBU("Entry renamed in the index..");
}

void FileManager::chunkHashKnown(const QSharedPointer<Chunk>& chunk)
{
   L_DEBU(QString("Adding chunk '%1' to the index..").arg(chunk->getHash().toStr()));
   this->chunks.add(chunk);
   L_DEBU("Chunk added to the index..");
   this->setCacheChanged();
}

void FileManager::chunkRemoved(const QSharedPointer<Chunk>& chunk)
{
   L_DEBU(QString("Removing chunk '%1' from the index ..").arg(chunk->getHash().toStr()));
   this->chunks.rm(chunk);
   L_DEBU("Chunk removed from the index..");
   this->setCacheChanged();
}

/**
  * Load the cache from a file. Called at start, by the constructor.
  * It will give the file cache to the fileUpdater and ask it
  * to load the cache.
  * It will also start the timer to persist the cache.
  */
void FileManager::loadCacheFromFile()
{
   // This hashes will be unallocated by the fileUpdater.
   Protos::FileCache::Hashes* savedCache = new Protos::FileCache::Hashes();

   try
   {
      Common::PersistentData::getValue(Common::Constants::FILE_CACHE, *savedCache, Common::Global::DataFolderType::LOCAL);
      if (static_cast<int>(savedCache->version()) != FILE_CACHE_VERSION)
      {
         L_ERRO(QString("The version (%1) of the file cache \"%2\" doesn't match the current version (%3)").arg(savedCache->version()).arg(Common::Constants::FILE_CACHE).arg(FILE_CACHE_VERSION));
         Common::PersistentData::rmValue(Common::Constants::FILE_CACHE, Common::Global::DataFolderType::LOCAL);
         delete savedCache;
         return;
      }

      // Scan the shared directories and try to match the files against the saved cache.
      try
      {
         this->cache.createSharedDirs(*savedCache);
      }
      catch (DirsNotFoundException& e)
      {
         foreach (QString path, e.paths)
            L_WARN(QString("During the file cache loading, this directory hasn't been found : %1").arg(path));
      }
   }
   catch (Common::UnknownValueException& e)
   {
      L_WARN(QString("The persisted file cache cannot be retrived (the file doesn't exist) : %1").arg(Common::Constants::FILE_CACHE));
   }
   catch (...)
   {
      L_WARN(QString("The persisted file cache cannot be retrived (Unkown exception) : %1").arg(Common::Constants::FILE_CACHE));
   }

   this->fileUpdater.setFileCache(savedCache);
}

/**
  * Save the cache to a file.
  * Called by the fileUpdater when it needs to persist the cache.
  */
void FileManager::persistCacheToFile()
{
   QMutexLocker locker(&this->mutexPersistCache);

   QMutexLocker lockerCacheChanged(&this->mutexCacheChanged);
   if (this->cacheChanged && !this->cacheLoading)
   {
      lockerCacheChanged.unlock();

      L_DEBU("Persisting cache..");

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
}

void FileManager::forcePersistCacheToFile()
{
   this->mutexCacheChanged.lock();
   this->cacheChanged = true;
   this->mutexCacheChanged.unlock();

   QMutexLocker locker(&this->mutexPersistCache);
   this->persistCacheToFile();
   this->timerPersistCache.start();
}

/**
  * @warning Can be called from differents thread like a 'Downloader' or the 'FileUpdater'.
  */
void FileManager::setCacheChanged()
{
   QMutexLocker locker(&this->mutexCacheChanged);
   this->cacheChanged = true;
}

void FileManager::fileCacheLoadingComplete()
{
   this->timerPersistCache.start();
   this->cacheLoading = false;

   emit fileCacheLoaded();
}
