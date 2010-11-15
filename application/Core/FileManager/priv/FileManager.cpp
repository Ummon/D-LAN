#include <priv/FileManager.h>
using namespace FM;

#include <QSharedPointer>
#include <QStringList>
#include <QList>
#include <QDir>
#include <QRegExp>
#include <QMutableListIterator>

#include <google/protobuf/text_format.h>

#include <Protos/files_cache.pb.h>

#include <Common/PersistantData.h>
#include <Common/Settings.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/GetHashesResult.h>
#include <priv/Cache/Entry.h>
#include <priv/Cache/File.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Chunk.h>

FileManager::FileManager()
   : CHUNK_SIZE(SETTINGS.get<quint32>("chunk_size")), fileUpdater(this), cache(this)
{
   connect(&this->cache, SIGNAL(entryAdded(Entry*)),     this, SLOT(entryAdded(Entry*)),     Qt::DirectConnection);
   connect(&this->cache, SIGNAL(entryRemoved(Entry*)),   this, SLOT(entryRemoved(Entry*)),   Qt::DirectConnection);
   connect(&this->cache, SIGNAL(chunkHashKnown(QSharedPointer<Chunk>)), this, SLOT(chunkHashKnown(QSharedPointer<Chunk>)), Qt::DirectConnection);
   connect(&this->cache, SIGNAL(chunkRemoved(QSharedPointer<Chunk>)),   this, SLOT(chunkRemoved(QSharedPointer<Chunk>)),   Qt::DirectConnection);

   connect(&this->cache, SIGNAL(newSharedDirectory(SharedDirectory*)),                 &this->fileUpdater, SLOT(addRoot(SharedDirectory*)),              Qt::DirectConnection);
   connect(&this->cache, SIGNAL(sharedDirectoryRemoved(SharedDirectory*, Directory*)), &this->fileUpdater, SLOT(rmRoot(SharedDirectory*, Directory*)),   Qt::DirectConnection);

   connect(&this->fileUpdater, SIGNAL(persistCache()),    this, SLOT(persistCacheToFile()), Qt::DirectConnection);
   connect(&this->fileUpdater, SIGNAL(fileCacheLoaded()), this, SIGNAL(fileCacheLoaded()),  Qt::QueuedConnection);

   this->loadCacheFromFile();

   this->fileUpdater.start();
}

FileManager::~FileManager()
{
   L_DEBU("~FileManager : Stopping the file updater..");
   this->fileUpdater.stop();
   L_DEBU("FileManager deleted");
}

/**
  * @exception SuperDirectoryExistsException Thrown when a super shared directory already exists.
  * @exception SubDirectoriesWithDifferentRightsExistsException Thrown when one or more sub directory already exists with different rights.
  * @exception SuperDirectoryExistsException Thrown when a super directory already exists regardless of the rights.
  */
void FileManager::setSharedDirsReadOnly(const QStringList& dirs)
{
   this->cache.setSharedDirs(dirs, SharedDirectory::READ_ONLY);
}

/**
  * @exception SuperDirectoryExistsException Thrown when a super shared directory already exists.
  * @exception SubDirectoriesWithDifferentRightsExistsException Thrown when one or more sub directory already exists with different rights.
  * @exception SuperDirectoryExistsException Thrown when a super directory already exists regardless of the rights.
  */
void FileManager::setSharedDirsReadWrite(const QStringList& dirs)
{
   this->cache.setSharedDirs(dirs, SharedDirectory::READ_WRITE);
}

QStringList FileManager::getSharedDirsReadOnly()
{
   return this->cache.getSharedDirs(SharedDirectory::READ_ONLY);
}

QStringList FileManager::getSharedDirsReadWrite()
{
   return this->cache.getSharedDirs(SharedDirectory::READ_WRITE);
}

QSharedPointer<IChunk> FileManager::getChunk(const Common::Hash& hash)
{
   return this->chunks.value(hash);
}

QList< QSharedPointer<IChunk> > FileManager::newFile(const Protos::Common::Entry& remoteEntry)
{
   return this->cache.newFile(remoteEntry);
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
  */
QList<Protos::Common::FindResult> FileManager::find(const QString& words, int maxNbResult, int maxSize)
{
   QStringList terms = FileManager::splitInWords(words);
   const int n = terms.size();

   // Launch a search for each term.
   QSet< NodeResult<Entry*> > results[n];
   for (int i = 0; i < n; i++)
      results[i] += this->wordIndex.search(terms[i]);

   QList<Protos::Common::FindResult> findResults;
   findResults << Protos::Common::FindResult();

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
      QList< NodeResult<Entry*> > nodesToSort;
      const int nCombinations = Common::Global::nCombinations(n, nbIntersect);
      for (int j = 0; j < nCombinations && !end; j++)
      {
         QSet< NodeResult<Entry*> > currentLevelSet;

         // Apply intersects.
         currentLevelSet = results[intersect[0]];

         for (QSetIterator< NodeResult<Entry*> > k(currentLevelSet); k.hasNext();)
         {
            NodeResult<Entry*>& node = const_cast<NodeResult<Entry*>&>(k.next());
            node.level = node.level ? nCombinations : 0;
         }

         for (int k = 1; k < nbIntersect; k++)
            NodeResult<Entry*>::intersect(currentLevelSet, results[intersect[k]], nCombinations);

         // Apply substracts.
         for (int k = -1; k < nbIntersect; k++)
         {
            for (
               int l = (k == -1 ? 0 : intersect[k] + 1);
                l < (k == nbIntersect - 1 ? n : intersect[k+1]);
                l++
            )
               currentLevelSet -= results[l];
         }

         for (QSetIterator< NodeResult<Entry*> > k(currentLevelSet); k.hasNext();)
            const_cast<NodeResult<Entry*>&>(k.next()).level += level;

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

      qSort(nodesToSort);

      // Populate the result.
      for (QListIterator< NodeResult<Entry*> > k(nodesToSort); k.hasNext();)
      {
         NodeResult<Entry*> entry = k.next();
         Protos::Common::FindResult_EntryLevel* entryLevel = findResults.last().add_entry();
         entryLevel->set_level(entry.level);
         entry.value->populateEntry(entryLevel->mutable_entry(), true);

         if (findResults.last().ByteSize() > maxSize)
         {
            google::protobuf::RepeatedPtrField<Protos::Common::FindResult_EntryLevel>* entries = findResults.last().mutable_entry();
            findResults << Protos::Common::FindResult();
            if (entries->size() > 0)
            {
               findResults.last().add_entry()->CopyFrom(entries->Get(entries->size()-1));
               entries->RemoveLast();
            }
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
   QBitArray result(hashes.size());
   for (int i = 0; i < hashes.size(); i++)
      result.setBit(i, this->chunks.contains(hashes[i]));
   return result;
}

quint64 FileManager::getAmount()
{
   return this->cache.getAmount();
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

void FileManager::entryAdded(Entry* entry)
{
   if (entry->getName().isEmpty() || Cache::isFileUnfinished(entry->getName()))
      return;

   L_DEBU(QString("Adding entry '%1' to the index ..").arg(entry->getName()));
   this->wordIndex.addItem(FileManager::splitInWords(entry->getName()), entry);
}

void FileManager::entryRemoved(Entry* entry)
{
   if (entry->getName().isEmpty())
      return;

   L_DEBU(QString("Removing entry '%1' from the index..").arg(entry->getName()));
   this->wordIndex.rmItem(FileManager::splitInWords(entry->getName()), entry);
}

void FileManager::chunkHashKnown(QSharedPointer<Chunk> chunk)
{
   L_DEBU(QString("Adding chunk '%1' to the index..").arg(chunk->getHash().toStr()));
   this->chunks.add(chunk);
}

void FileManager::chunkRemoved(QSharedPointer<Chunk> chunk)
{
   L_DEBU(QString("Removing chunk '%1' from the index ..").arg(chunk->getHash().toStr()));
   this->chunks.rm(chunk);
}

/**
  * Take raw terms in a string and split, trim and filter to
  * return a list of keyword.
  * Some character or word can be removed.
  * @example " The little  DUCK " => ["little", "duck"].
  */
QStringList FileManager::splitInWords(const QString& words)
{
   const static QRegExp regExp("(\\W+|_)");
   return words.toLower().split(regExp, QString::SkipEmptyParts);
}

/**
  * Load the cache from a file. Called at start, by the constructor.
  * It will give the file cache to the fileUpdater and ask it
  * to load the cache.
  */
void FileManager::loadCacheFromFile()
{
   // This hashes will be unallocated by the fileUpdater.
   Protos::FileCache::Hashes* savedCache = new Protos::FileCache::Hashes();

   try
   {
      Common::PersistantData::getValue(Common::FILE_CACHE, *savedCache);
      if (static_cast<int>(savedCache->version()) != FILE_CACHE_VERSION)
      {
         L_ERRO(QString("The version (%1) of the file cache \"%2\" doesn't match the current version (%3)").arg(savedCache->version()).arg(Common::FILE_CACHE).arg(FILE_CACHE_VERSION));
         Common::PersistantData::rmValue(Common::FILE_CACHE);
         return;
      }

      // Scan the shared directories and try to match the files against the saved cache.
      try
      {
         this->cache.retrieveFromFile(*savedCache);
      }
      catch (DirsNotFoundException& e)
      {
         foreach (QString path, e.paths)
            L_WARN(QString("During the file cache loading, this directory hasn't been found : %1").arg(path));
      }
   }
   catch (Common::UnknownValueException& e)
   {
      L_WARN(QString("The persisted file cache cannot be retrived (the file doesn't exist) : %1").arg(Common::FILE_CACHE));
   }
   catch (...)
   {
      L_WARN(QString("The persisted file cache cannot be retrived (Unkown exception) : %1").arg(Common::FILE_CACHE));
   }

   this->fileUpdater.setFileCache(savedCache);
}

/**
  * Save the cache to a file.
  * Called by the fileUpdater when it needs to persist the cache.
  * @warning Called in the fileUpdater thread.
  */
void FileManager::persistCacheToFile()
{
   L_DEBU("Persists cache..");
   L_DEBU("#######################################");

   Protos::FileCache::Hashes hashes;
   this->cache.saveInFile(hashes);

   try
   {
      Common::PersistantData::setValue(Common::FILE_CACHE, hashes);
   }
   catch (Common::PersistantDataIOException& err)
   {
      L_ERRO(err.message);
   }
}
