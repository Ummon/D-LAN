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
#include <Exceptions.h>
#include <priv/Global.h>
#include <priv/Constants.h>
#include <priv/GetHashesResult.h>
#include <priv/Cache/Entry.h>
#include <priv/Cache/File.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>

LOG_INIT_CPP(FileManager);

FileManager::FileManager() :
   CHUNK_SIZE(SETTINGS.get<quint32>("chunk_size")),
   fileUpdater(this),
   cache(),
   mutexPersistCache(QMutex::Recursive),
   cacheLoading(true),
   cacheChanged(false)
{
   connect(&this->cache, SIGNAL(entryAdded(Entry*)),     this, SLOT(entryAdded(Entry*)),     Qt::DirectConnection);
   connect(&this->cache, SIGNAL(entryRemoved(Entry*)),   this, SLOT(entryRemoved(Entry*)),   Qt::DirectConnection);
   connect(&this->cache, SIGNAL(chunkHashKnown(QSharedPointer<Chunk>)), this, SLOT(chunkHashKnown(QSharedPointer<Chunk>)), Qt::DirectConnection);
   connect(&this->cache, SIGNAL(chunkRemoved(QSharedPointer<Chunk>)),   this, SLOT(chunkRemoved(QSharedPointer<Chunk>)),   Qt::DirectConnection);

   connect(&this->cache, SIGNAL(newSharedDirectory(SharedDirectory*)),                 this, SLOT(newSharedDirectory(SharedDirectory*)),                 Qt::DirectConnection);
   connect(&this->cache, SIGNAL(sharedDirectoryRemoved(SharedDirectory*, Directory*)), this, SLOT(sharedDirectoryRemoved(SharedDirectory*, Directory*)), Qt::DirectConnection);

   connect(&this->fileUpdater, SIGNAL(fileCacheLoaded()), this, SLOT(fileCacheLoadingComplete()),  Qt::QueuedConnection);

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

QList< QSharedPointer<IChunk> > FileManager::getAllChunks(const Protos::Common::Entry& localEntry, const Common::Hashes& hashes) const
{
   for (QListIterator<Common::Hash> h(hashes); h.hasNext();)
   {
      // Chunks from different files, usually one chunk.
      QList< QSharedPointer<Chunk> > chunks = this->chunks.values(h.next());

      for (QListIterator< QSharedPointer<Chunk> > i(chunks); i.hasNext();)
      {
         QSharedPointer<Chunk> chunk = i.next();
         if (chunk->matchesEntry(localEntry)) // The name, the path and the size of the file are the same?
         {
            // We verify that all hashes of all chunks match the given hashes. If it's not the case, the files are not the same.
            QList< QSharedPointer<Chunk> > allChunks = chunk->getOtherChunks();
            if (allChunks.size() != hashes.size())
               return QList< QSharedPointer<IChunk> >();

            QList< QSharedPointer<IChunk> > ret;
            for (int j = 0; j < allChunks.size(); j++)
            {
               if (allChunks[j]->getHash() != hashes[j]) // Only one hashes doesn't match -> all the file doesn't match.
                  return QList< QSharedPointer<IChunk> >();
               ret << allChunks[j];
            }
            return ret;
         }
      }
   }

   return QList< QSharedPointer<IChunk> >();
}

QList< QSharedPointer<IChunk> > FileManager::newFile(Protos::Common::Entry& entry)
{
   return this->cache.newFile(entry);
}

QSharedPointer<IGetHashesResult> FileManager::getHashes(const Protos::Common::Entry& file)
{
   return QSharedPointer<IGetHashesResult>(new GetHashesResult(file, this->cache, this->fileUpdater));
}

Protos::Common::Entries FileManager::getEntries(const Protos::Common::Entry& dir)
{
   return this->cache.getEntries(dir);
}

Protos::Common::Entries FileManager::getEntries()
{
   return this->cache.getEntries();
}

/**
  * @see http://dev.euphorik.ch/wiki/pmp/Algorithms#Word-indexing for more information.
  * TODO: A large part of this function code should be owned by 'WordIndex'.
  */
QList<Protos::Common::FindResult> FileManager::find(const QString& words, int maxNbResult, int maxSize)
{
   QStringList terms = Common::Global::splitInWords(words);
   const int n = terms.size();

   // Launch a search for each term.
   QVector<QSet< NodeResult<Entry> > > results(n);
   for (int i = 0; i < n; i++)
      // We can only limit the number of result for one term. When there is more than one term and thus some results set, say [a, b, c] for example, some good result may be contained in intersect, for example a & b or a & c.
      results[i] += this->wordIndex.search(terms[i], n == 1 ? maxNbResult : -1).toSet();

   QList<Protos::Common::FindResult> findResults;
   findResults << Protos::Common::FindResult();
   findResults.last().set_tag(std::numeric_limits<quint64>::max()); // Worst case to compute the size.

   const int constantFindResultsSize = findResults.last().ByteSize();
   int findResultCurrentSize = constantFindResultsSize; // [Byte].

   int numberOfResult = 0;
   bool end = false;

   int level = 0;
   // For each group of intersection number.
   // For example, [a, b, c] :
   //  * a & b & c
   //  * (a & b) \ c
   //    (a & c) \ b
   //    (b & c) \ a
   //  * a \ b \ c
   for (int i = 0; i < n && !end; i++)
   {
      const int nbIntersect = n - i; // Number of set intersected.
      int intersect[nbIntersect]; // A array of the results wich will be intersected.
      for (int j = 0; j < nbIntersect; j++)
         intersect[j] = j;

      // For each combination of the current intersection group.
      // For 2 intersections (nbIntersect == 2) among 3 elements [a, b, c] :
      //  * (a, b)
      //  * (a, c)
      //  * (b, c)
      QList< NodeResult<Entry> > nodesToSort;
      const int nCombinations = Common::Global::nCombinations(n, nbIntersect);
      for (int j = 0; j < nCombinations && !end; j++)
      {
         QSet< NodeResult<Entry> > currentLevelSet;

         // Apply intersects.
         currentLevelSet = results[intersect[0]];

         for (QSetIterator< NodeResult<Entry> > k(currentLevelSet); k.hasNext();)
         {
            NodeResult<Entry>& node = const_cast<NodeResult<Entry>&>(k.next());
            node.level = node.level ? nCombinations : 0;
         }

         for (int k = 1; k < nbIntersect; k++)
            NodeResult<Entry>::intersect(currentLevelSet, results[intersect[k]], nCombinations);

         // Apply substracts.
         for (int k = -1; k < nbIntersect; k++)
            for (int l = (k == -1 ? 0 : intersect[k] + 1); l < (k == nbIntersect - 1 ? n : intersect[k+1]); l++)
               currentLevelSet -= results[l];

         for (QSetIterator< NodeResult<Entry> > k(currentLevelSet); k.hasNext();)
            const_cast<NodeResult<Entry>&>(k.next()).level += level;

         // Sort by level.
         nodesToSort << currentLevelSet.toList();

         // Define positions of each intersect term.
         for (int k = nbIntersect - 1; k >= 0; k--)
            if  (intersect[k] < n - nbIntersect + k)
            {
               intersect[k] += 1;
               for (int l = k + 1; l < nbIntersect; l++)
                  intersect[l] = intersect[k] + (l - k);
               break;
            }

         level += 1;
      }

      qSort(nodesToSort); // Sort by level

      // Populate the result.
      for (QListIterator< NodeResult<Entry> > k(nodesToSort); k.hasNext();)
      {
         NodeResult<Entry> entry = k.next();
         Protos::Common::FindResult_EntryLevel* entryLevel = findResults.last().add_entry();
         entryLevel->set_level(entry.level);
         entry.value->populateEntry(entryLevel->mutable_entry(), true);

         // We wouldn't use 'findResults.last().ByteSize()' because is too slow. Instead we call 'ByteSize()' for each entry and sum it.
         const int entryByteSize = entryLevel->ByteSize() + 8; // Each entry take a bit of memory... (Value found in an empiric way..).
         findResultCurrentSize += entryByteSize;

         if (findResultCurrentSize > maxSize)
         {
            google::protobuf::RepeatedPtrField<Protos::Common::FindResult_EntryLevel>* entries = findResults.last().mutable_entry();
            findResults << Protos::Common::FindResult();
            if (entries->size() > 0)
            {
               findResults.last().add_entry()->CopyFrom(entries->Get(entries->size()-1));
               entries->RemoveLast();
            }
            findResultCurrentSize = constantFindResultsSize + entryByteSize;
         }

         if (++numberOfResult >= maxNbResult)
         {
            end = true;
            break;
         }
      }

      level += nCombinations * nbIntersect;
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

void FileManager::entryAdded(Entry* entry)
{
   if (entry->getName().isEmpty() || Global::isFileUnfinished(entry->getName()))
      return;

   L_DEBU(QString("Adding entry '%1' to the index ..").arg(entry->getName()));
   this->wordIndex.addItem(Common::Global::splitInWords(entry->getName()), entry);
   L_DEBU("Entry added to the index ..");
}

void FileManager::entryRemoved(Entry* entry)
{
   if (entry->getName().isEmpty())
      return;

   L_DEBU(QString("Removing entry '%1' from the index..").arg(entry->getName()));
   this->wordIndex.rmItem(Common::Global::splitInWords(entry->getName()), entry);
   L_DEBU("Entry removed from the index..");
}

void FileManager::chunkHashKnown(QSharedPointer<Chunk> chunk)
{
   L_DEBU(QString("Adding chunk '%1' to the index..").arg(chunk->getHash().toStr()));
   this->chunks.add(chunk);
   L_DEBU("Chunk added to the index..");
   this->setCacheChanged();
}

void FileManager::chunkRemoved(QSharedPointer<Chunk> chunk)
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
      Common::PersistentData::getValue(Common::Constants::FILE_CACHE, *savedCache, Common::Global::LOCAL);
      if (static_cast<int>(savedCache->version()) != FILE_CACHE_VERSION)
      {
         L_ERRO(QString("The version (%1) of the file cache \"%2\" doesn't match the current version (%3)").arg(savedCache->version()).arg(Common::Constants::FILE_CACHE).arg(FILE_CACHE_VERSION));
         Common::PersistentData::rmValue(Common::Constants::FILE_CACHE, Common::Global::LOCAL);
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
         Common::PersistentData::setValue(Common::Constants::FILE_CACHE, hashes, Common::Global::LOCAL);
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
