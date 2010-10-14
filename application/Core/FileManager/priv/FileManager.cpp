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

   connect(&this->fileUpdater, SIGNAL(persistCache()), this, SLOT(persistCacheToFile()), Qt::DirectConnection);

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
   QSharedPointer<IChunk> chunk = this->chunks.value(hash);
   if (chunk.isNull())
      throw UnknownChunkException();
   return chunk;
}

QList< QSharedPointer<IChunk> > FileManager::newFile(const Protos::Common::Entry& remoteEntry)
{
   return this->cache.newFile(remoteEntry);
}

QSharedPointer<IGetHashesResult> FileManager::getHashes(const Protos::Common::Entry& file)
{
   return QSharedPointer<IGetHashesResult>(new GetHashesResult(file, this->cache, this->fileUpdater));
}

Protos::Core::GetEntriesResult FileManager::getEntries(const Protos::Common::Entry& dir)
{
   return this->cache.getEntries(dir);
}

Protos::Core::GetEntriesResult FileManager::getEntries()
{
   return this->cache.getEntries();
}

/**
  * @see http://dev.euphorik.ch/wiki/pmp/Algorithms#Word-indexing for more information.
  */
QList<Protos::Common::FindResult> FileManager::find(const QString& words, int maxNbResult, int maxSize)
{
   QStringList terms = FileManager::splitInWords(words);
   int n = terms.size();

   // Launch a search for each term.
   QSet<Entry*> results[n];
   for (int i = 0; i < n; i++)
      results[i] += this->wordIndex.search(terms[i]);

   QList<Protos::Common::FindResult> findResults;
   findResults << Protos::Common::FindResult();

   int numberOfResult = 0;
   bool end = false;

   int level = 0;
   // For each group of intersection number.
   // For example, [a, b, c] :
   //  1) a & b & c
   //  2) (a & b) \ c
   //     (a & c) \ b
   //     (b & c) \ a
   //  3) a \ b \ c
   for (int i = 0; i < n && !end; i++)
   {
      int nbIntersect = n - i; // Number of set intersected.
      int intersect[nbIntersect]; // A array of the results wich will be intersected.
      for (int j = 0; j < nbIntersect; j++)
         intersect[j] = j;

      // For each combination of the current intersection group.
      // For 2 intersections (nbIntersect == 2) among 3 elements [a, b, c] :
      //  * (a, b)
      //  * (a, c)
      //  * (b, c)
      for (int j = 0; j < Common::Global::nCombinations(n, nbIntersect) && !end; j++)
      {
         QSet<Entry*> currentLevelSet;

         // Apply intersects.
         currentLevelSet = results[intersect[0]];
         for (int k = 1; k < nbIntersect; k++)
            currentLevelSet &= results[intersect[k]];

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

         // Populate the result.
         for (QSetIterator<Entry*> k(currentLevelSet); k.hasNext();)
         {
            Entry* entry = k.next();
            Protos::Common::FindResult_EntryLevel* entryLevel = findResults.last().add_entry();
            entryLevel->set_level(level);
            entry->populateEntry(entryLevel->mutable_entry());

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
   QStringList keywords = words.toLower().split(regExp, QString::SkipEmptyParts);

   // Remove all word smaller than MAX_WORD_LENGTH.
   int i = 0;
   while (i < keywords.length())
   {
      if (keywords[i].length() < MAX_WORD_LENGTH)
         keywords.removeAt(i);
      else
         i += 1;
   }

   return keywords;
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
   //LOG_DEBUG(QString("hashes.SpaceUsed() = %1").arg(hashes.SpaceUsed()));
   Common::PersistantData::setValue(Common::FILE_CACHE, hashes);
}
