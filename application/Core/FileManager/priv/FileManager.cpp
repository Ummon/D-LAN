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
   cache(hashCache)
   // cacheLoading(true)
{
   Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;

   connect(&this->cache, &Cache::entryAdded, this, &FileManager::entryAdded, Qt::DirectConnection);
   connect(&this->cache, &Cache::entryRemoved, this, &FileManager::entryRemoved, Qt::DirectConnection);
   connect(&this->cache, &Cache::entryRenamed, this, &FileManager::entryRenamed, Qt::DirectConnection);
   connect(&this->cache, &Cache::chunkHashKnown, this, &FileManager::chunkHashKnown, Qt::DirectConnection);
   connect(&this->cache, &Cache::chunkRemoved, this, &FileManager::chunkRemoved, Qt::DirectConnection);

   connect(&this->cache, &Cache::newSharedEntry, this, &FileManager::newSharedEntry, Qt::DirectConnection);
   connect(&this->cache, &Cache::sharedEntryRemoved, this, &FileManager::sharedEntryRemoved, Qt::DirectConnection);

   connect(
      &this->fileUpdater,
      &FileUpdater::initialScanFinished,
      this,
      &FileManager::setInitialFileCacheScanningComplete,
      Qt::ConnectionType(Qt::SingleShotConnection | Qt::QueuedConnection)
   );

   // If the 'FileUpdater' wants to delete a shared directory.
   connect(&this->fileUpdater, &FileUpdater::deleteSharedEntry, this, &FileManager::deleteSharedEntry, Qt::QueuedConnection);

   connect(&this->cache, &Cache::fileResizing, this, &FileManager::fileResizing, Qt::DirectConnection);
   connect(&this->cache, &Cache::fileResized, this, &FileManager::fileResized, Qt::DirectConnection);

   // Give stored shared entries to the cache:
   for (const auto& entry : SETTINGS.getRepeated<Protos::Common::SharedEntry>("shared_entries"))
   {
      try
      {
         this->cache.addExistingSharedEntry(entry);
      }
      catch (EntriesNotFoundException e)
      {
         L_WARN(QString("Unable to add shared entry: %1").arg(e.paths.constFirst()));
      }
   }

   this->fileUpdater.start();
}

FileManager::~FileManager()
{
   this->fileUpdater.stop();
   this->cache.disconnect(this);
   L_DEBU("FileManager deleted");
}

/**
  * @exception EntriesNotFoundException
  */
void FileManager::setSharedPaths(const QList<IFileManager::SharedPath>& paths)
{
   QList<std::pair<QString, Common::Path>> commonPaths;
   for (const auto& path : paths)
      commonPaths << std::make_pair(path.name, Common::Path(path.path));
   this->cache.setSharedPaths(commonPaths);
}

/**
  * @exception EntriesNotFoundException
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
      return entry->getPath().toString();
   else
      return QString();
}

QSharedPointer<IChunk> FileManager::getChunk(const Common::Hash& hash) const
{
   return this->chunks.value(hash);
}

QList<QSharedPointer<IChunk>> FileManager::getAllChunks(
   const Protos::Common::Entry& localEntry,
   const QList<Common::Hash>& hashes
) const
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
               // Only one hashes doesn't match -> all the file doesn't match.
               if (
                  !allChunks[j]->getHash().isNull() &&
                  !hashes[j].isNull() &&
                  allChunks[j]->getHash() != hashes[j]
               )
                  return QList<QSharedPointer<IChunk>>();

               ret << allChunks[j];
            }
            return ret;
         }
      }
   }

   return QList<QSharedPointer<IChunk>>();
}

void FileManager::updateFromQueueEntry(const Protos::Queue::Queue_Entry& entry)
{
   Protos::Common::Entry localEntryCopy(entry.local_entry());
   const QString entryName = QString::fromStdString(localEntryCopy.name());
   if (!Global::isFileUnfinished(entryName))
   {
      const QString entryNameUnfinished = entryName + Global::getUnfinishedSuffix();
      localEntryCopy.set_name(entryNameUnfinished.toStdString());
   }

   File* file = this->cache.getFile(localEntryCopy);

   if (file)
   {
      file->setHidden(entry.remote_entry().hidden());

      auto chunks = file->getChunks();
      for (int i = 0; i < chunks.size() && i < entry.remote_entry().chunks_size() && i < entry.known_bytes_size(); ++i)
      {
         const Common::Hash hash(entry.remote_entry().chunks(i).hash());
         if (!hash.isNull())
            chunks[i]->setHash(hash, false);

         if (entry.known_bytes(i) > 0)
         {
            chunks[i]->setKnownBytes(entry.known_bytes(i));
            if (chunks[i]->isComplete())
               file->chunkComplete(chunks[i].data());
         }
      }
   }
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

QList<Protos::Common::FindResult> FileManager::find(
   const QString& words,
   const QList<QString>& extensions,
   qint64 minFileSize,
   qint64 maxFileSize,
   Protos::Common::FindPattern_Category category,
   int maxNbResult,
   int maxSize,
   bool setSharedEntryPath
)
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
            Common::StringUtils::splitInWords(words),
            maxNbResult,
            [&](const Entry* entry) {
               const File* file = dynamic_cast<const File*>(entry);
               return (!filterBySizeOn || entry->getSize() >= minFileSize && entry->getSize() <= maxFileSize) &&
                      (!filterByExtensionsOn || file && extensions.contains(file->getExtension().toLower())) &&
                      (!filterByCategoryOn || (category == Protos::Common::FindPattern::FILE && dynamic_cast<const File*>(entry) || category == Protos::Common::FindPattern::DIR && dynamic_cast<const Directory*>(entry)));
            }
         );
   }
   // We cannot find by extension or by size if the category is only directory.
   else if ((filterBySizeOn || filterByExtensionsOn) && category != Protos::Common::FindPattern::DIR)
   {
      QList<File*> intermediateResult;

      if (!extensions.isEmpty())
      {
         if (filterBySizeOn)
            intermediateResult =
               this->extensionIndex.search(
                  extensions,
                  maxNbResult,
                  [&](const File* file)
                  {
                     return (file->getSize() >= minFileSize && file->getSize() <= maxFileSize);
                  }
               );
         else
            intermediateResult = this->extensionIndex.search(extensions, maxNbResult);
      }
      else
      {
         for (auto file : this->sizeIndex.search(minFileSize, maxFileSize, maxNbResult))
            intermediateResult << dynamic_cast<File*>(file);
      }

      for (QListIterator<File*> i(intermediateResult); i.hasNext();)
         result << NodeResult<Entry*>(i.next());
   }

   QList<Protos::Common::FindResult> findResults;
   findResults << Protos::Common::FindResult();
   // Worst case to compute the size (int fields have a variable size).
   findResults.last().set_tag(std::numeric_limits<quint64>::max());

   const int EMPTY_FIND_RESULT_SIZE = findResults.last().ByteSizeLong(); // Around ~11 bytes.
   int findResultCurrentSize = EMPTY_FIND_RESULT_SIZE; // [Byte].

   for (QListIterator<NodeResult<Entry*>> i(result); i.hasNext();)
   {
      const NodeResult<Entry*>& entry = i.next();
      Protos::Common::FindResult::EntryLevel* entryLevel = findResults.last().add_entries();
      entryLevel->set_level(entry.level);

      File* file = dynamic_cast<File*>(entry.value);
      if (file)
         file->populateEntry(entryLevel->mutable_entry(), true, NB_MAX_HASHES_PER_ENTRY_SEARCH);
      else
         entry.value->populateEntry(entryLevel->mutable_entry(), true);

      if (!setSharedEntryPath)
         entryLevel->mutable_entry()->mutable_shared_entry()->clear_path();

      // We wouldn't use 'findResults.last().ByteSizeLong()' because is too slow.
      // Instead we call 'ByteSize()' for each entry and sum it.
      const int entryByteSize = entryLevel->ByteSizeLong() + 8; // Each entry take a bit of memory overhead (value found in an empiric way).
      findResultCurrentSize += entryByteSize;

      // If the last result is too big the last entry of the last result will be put in a new result.
      if (findResultCurrentSize > maxSize && findResults.constLast().entries_size() > 1)
      {
         findResults << Protos::Common::FindResult();

         // Move the last entry to the new result.
         findResults.last().mutable_entries()->AddAllocated(
            findResults[findResults.size() - 2].mutable_entries()->ReleaseLast()
         );

         findResultCurrentSize = EMPTY_FIND_RESULT_SIZE + entryByteSize;
      }
   }

   if (findResults.last().entries_size() == 0)
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

qint64 FileManager::getAmount()
{
   return this->cache.getAmount();
}

FileManager::CacheStatus FileManager::getCacheStatus() const
{
   if (!this->initialFileCacheScanningComplete)
      return INITIAL_SCANNING_IN_PROGRESS;

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

QString FileManager::getWordIndex_debug() const
{
   return this->wordIndex.toStringLog();
}

/**
  * Incomplete, only the first hash is compared for the moment.
  */
QString FileManager::getSimilarFiles_debug() const
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

   return result;
}

QString FileManager::getCacheTree_debug() const
{
   return this->cache.getTree_debug();
}

Directory* FileManager::getFittestDirectory(const QString& path)
{
   return this->cache.getFittestDirectory(path);
}

/**
  * Used to retrieve a file or a directory by the fileUpdater when a filesystem event occurs.
  */
Entry* FileManager::getEntry(const Common::Path& path) const
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
}

void FileManager::deleteSharedEntry(SharedEntry* sharedEntry)
{
   this->cache.removeSharedEntry(sharedEntry);
}

void FileManager::entryAdded(Entry* entry)
{
   if (Global::isFileUnfinished(entry->getName()))
      return;

   const QString name = entry->getUserName();

   if (name.isEmpty())
      return;

   L_DEBU(QString("Adding entry '%1' to the index . . .").arg(name));

   this->wordIndex.addItem(Common::StringUtils::splitInWords(name), entry);

   if (File* file = dynamic_cast<File*>(entry))
   {
      this->extensionIndex.addItem(file->getExtension(), file);
      this->sizeIndex.addItem(file); // TODO: Nedded?
   }
}

void FileManager::entryRemoved(Entry* entry)
{
   const QString name = entry->getUserName();

   if (name.isEmpty())
      return;

   L_DEBU(QString("Removing entry '%1' from the index . . .").arg(name));
   if (!this->wordIndex.rmItem(Common::StringUtils::splitInWords(name), entry))
      L_DEBU(QString("The entry '%1' hasn't been found in the index!").arg(name));

   if (File* file = dynamic_cast<File*>(entry))
   {
      this->extensionIndex.rmItem(file->getExtension(), file);
      this->sizeIndex.rmItem(file);
   }
}

void FileManager::entryRenamed(Entry* entry, const QString& oldName)
{
   const QString name = entry->getUserName();

   L_DEBU(QString("Renaming entry '%1' to '%2' in the index . . .").arg(name, oldName));

   this->wordIndex.renameItem(Common::StringUtils::splitInWords(oldName), Common::StringUtils::splitInWords(name), entry);

   if (File* file = dynamic_cast<File*>(entry))
      this->extensionIndex.changeItem(Common::KnownExtensions::getExtension(oldName), file->getExtension(), file);
}

void FileManager::fileResizing(File* file)
{
   this->sizeIndex.rmItem(file);
}

void FileManager::fileResized(File* file, qint64 oldSize)
{
   this->sizeIndex.addItem(file);
}

void FileManager::chunkHashKnown(const QSharedPointer<Chunk>& chunk)
{
   L_DEBU(QString("Adding chunk '%1' to the index . . .").arg(chunk->getHash().toStrShort()));
   this->chunks.add(chunk);
}

void FileManager::chunkRemoved(const QSharedPointer<Chunk>& chunk)
{
   L_DEBU(QString("Removing chunk '%1' from the index . . .").arg(chunk->getHash().toStrShort()));
   this->chunks.rm(chunk);
}

void FileManager::setInitialFileCacheScanningComplete()
{
   this->initialFileCacheScanningComplete = true;
   emit fileCacheScanningComplete();
}
