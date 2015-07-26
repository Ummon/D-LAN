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
  
#include <priv/FileUpdater/FileUpdater.h>
using namespace FM;

#include <QLinkedList>
#include <QDir>
#include <QElapsedTimer>

#include <Common/Settings.h>

#include <Exceptions.h>
#include <priv/Global.h>
#include <priv/Exceptions.h>
#include <priv/Log.h>
#include <priv/Constants.h>
#include <priv/FileManager.h>
#include <priv/Cache/SharedEntry.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>
#include <priv/FileUpdater/WaitCondition.h>

/**
  * @class FM::FileUpdater
  *
  */

FileUpdater::FileUpdater(FileManager* fileManager) :
   SCAN_PERIOD_UNWATCHABLE_DIRS(SETTINGS.get<quint32>("scan_period_unwatchable_dirs")),
   fileManager(fileManager),
   dirWatcher(DirWatcher::getNewWatcher()),
   fileCacheInformation(nullptr),
   toStop(false),
   progress(0),
   mutex(QMutex::Recursive),
   currentScanningEntry(nullptr),
   toStopHashing(false),
   remainingSizeToHash(0)
{
   this->dirEvent = WaitCondition::getNewWaitCondition();
}

FileUpdater::~FileUpdater()
{
   if (this->dirEvent)
      delete this->dirEvent;

   if (this->dirWatcher)
      delete this->dirWatcher;

   L_DEBU("FileUpdater deleted");
}

void FileUpdater::stop()
{
   L_DEBU("Stopping FileUpdater ..");

   this->toStop = true;

   L_DEBU("Stopping hashing ..");
   this->stopHashing();
   L_DEBU("Hashing stopped");

   L_DEBU("Stopping scanning ..");
   this->stopScanning();
   L_DEBU("Scanning stopped");

   // Simulate a dirEvent to stop the main loop.
   this->dirEvent->release();

   this->wait();
}

/**
  * Called by another thread.
  * @exception DirNotFoundException
  */
void FileUpdater::addRoot(SharedEntry* sharedEntry)
{
   QMutexLocker locker(&this->mutex);

   const Common::Path& entryPath = sharedEntry->getFullPath();

   bool watchable = false;
   if (this->dirWatcher)
      watchable = this->dirWatcher->addPath(entryPath.getPath());

   this->entriesToScan << sharedEntry->getRootEntry();

   if (!watchable)
   {
      L_WARN(QString("This entry is not watchable: %1").arg(entryPath.getPath()));
      this->unwatchableEntries << sharedEntry;
   }

   this->dirEvent->release();
}

/**
  * Called by another thread.
  * If 'dir2' is given it will steal the content of 'entry' and append
  * them to itself.
  */
void FileUpdater::rmRoot(SharedEntry* sharedEntry, Directory* dir2)
{
   // If there is a scanning for this directory stop it.
   this->stopScanning(sharedEntry->getRootEntry());

   QMutexLocker locker(&this->mutex);

   // Stop the hashing to modify 'this->fileWithoutHashes'.
   {
      QMutexLocker locker(&this->hashingMutex);

      this->fileHasher.stop();
      this->toStopHashing = true;

      // TODO: Find a more elegant way!
      Directory* rootDirectory = dynamic_cast<Directory*>(sharedEntry->getRootEntry());
      if (dir2 && rootDirectory)
         dir2->stealContent(rootDirectory);

      this->removeFromFilesWithoutHashes(sharedEntry->getRootEntry());
      this->removeFromDirsToScan(dir);
      this->unwatchableEntries.removeOne(dir);
      this->entriesToRemove << sharedEntry->getRootEntry();
   }

   this->dirEvent->release();
}

/**
  * Set the file cache to retrieve the hashes frome it.
  * Muste be called before starting the fileUpdater.
  * The shared dirs in fileCache must be previously added by 'addRoot(..)'.
  * This object must unallocated the hashes.
  */
/*void FileUpdater::setFileCache(const Protos::FileCache::Hashes* fileCache)
{
   this->fileCacheInformation = new FileCacheInformation(fileCache);
}*/

void FileUpdater::prioritizeAFileToHash(File* file)
{
   QMutexLocker locker(&this->mutex);
   L_DEBU(QString("FileUpdater::prioritizeAFileToHash : %1").arg(file->getFullPath()));

   // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
   if (!file->hasAllHashes() && file->isComplete())
   {
      QMutexLocker lockerHashing(&this->hashingMutex);

      if (this->filesWithoutHashes.removeOne(file))
      {
         this->remainingSizeToHash -= file->getSize();
      }

      if (!this->filesWithoutHashesPrioritized.contains(file))
      {
         this->filesWithoutHashesPrioritized << file;
         this->remainingSizeToHash += file->getSize();
      }

      // Commmented to avoid this behavior:
      // When a lot of unhashed tiny file are asked the hashing process will constently abort the current hashing file
      // and will never finish it thus slow down the global hashing rate.
      // this->fileHasher.stop();

      this->toStopHashing = true;
   }
   else
      L_DEBU(QString("FileUpdater::prioritizeAFileToHash, unable to prioritize : %1").arg(file->getFullPath()));

}

bool FileUpdater::isScanning() const
{
   QMutexLocker scanningLocker(&this->scanningMutex);
   return this->currentScanningEntry != nullptr;
}

bool FileUpdater::isHashing() const
{
   QMutexLocker locker(&this->mutex);
   return !this->filesWithoutHashes.isEmpty() || !this->filesWithoutHashesPrioritized.isEmpty();
}

int FileUpdater::getProgress() const
{
   QMutexLocker locker(&this->mutex);
   return this->progress;
}

void FileUpdater::run()
{
   this->timerScanUnwatchable.start();

   QString threadName = "FileUpdater";
#if DEBUG
   threadName.append("_").append(QString::number((intptr_t)QThread::currentThreadId()));
#endif
   QThread::currentThread()->setObjectName(threadName);

   // First: retrieve the directories and file from the file cache and
   // synchronize it with the file system.
   if (this->fileCacheInformation)
   {
      while (!this->entriesToScan.isEmpty())
      {
         Entry* entry = this->entriesToScan.takeFirst();
         this->scan(entry, true);
         // TODO:
         //this->restoreFromFileCache(static_cast<SharedDirectory*>(dir));
      }

      delete this->fileCacheInformation;
      this->fileCacheInformation = nullptr;
   }

   emit fileCacheLoaded();

   this->progress = 0;

   forever
   {
      this->computeSomeHashes();

      this->mutex.lock();

      foreach (Entry* entry, this->entriesToRemove)
      {
         L_DEBU(QString("Stop watching this path: %1").arg(entry->getFullPath()));
         if (this->dirWatcher)
            this->dirWatcher->rmPath(entry->getFullPath());

         entry->removeUnfinishedFiles();
         entry->del();
      }
      this->entriesToRemove.clear();

      // If there is no watcher capability or no directory to watch then
      // we wait for an added directory.
      if (!this->dirWatcher || this->dirWatcher->nbWatchedPath() == 0 || !this->dirsToScan.empty())
      {
         if (this->dirsToScan.isEmpty() && this->filesWithoutHashes.isEmpty() && this->filesWithoutHashesPrioritized.isEmpty())
         {
            L_DEBU("Waiting for a new shared directory added..");
            this->mutex.unlock();
            this->dirEvent->wait(this->unwatchableEntries.isEmpty() ? -1 : SCAN_PERIOD_UNWATCHABLE_DIRS);
         }
         else
            this->mutex.unlock();

         Directory* addedDir = nullptr;
         this->mutex.lock();
         if (!this->dirsToScan.isEmpty())
            addedDir = this->dirsToScan.takeLast();
         this->mutex.unlock();

         // Synchronize the new directory.
         if (addedDir)
         {
            this->scan(addedDir);
         }
      }
      else // Wait for filesystem modifications.
      {
         // If we have no dir to scan and no file to hash we wait for a new shared file
         // or a filesystem event.
         if (this->dirsToScan.isEmpty() && this->filesWithoutHashes.isEmpty() && this->filesWithoutHashesPrioritized.isEmpty())
         {
            this->mutex.unlock();
            this->processEvents(this->dirWatcher->waitEvent(this->unwatchableEntries.isEmpty() ? -1 : SCAN_PERIOD_UNWATCHABLE_DIRS, QList<WaitCondition*>() << this->dirEvent));
         }
         else
         {
            this->mutex.unlock();
            this->processEvents(this->dirWatcher->waitEvent(0)); // Just pick the new events. (Don't wait for new event).
         }
      }

      if (timerScanUnwatchable.elapsed() >= SCAN_PERIOD_UNWATCHABLE_DIRS)
      {
         this->mutex.lock();
         QList<Directory*> unwatchableEntriesCopy = this->unwatchableEntries;
         this->mutex.unlock();

         // Synchronize the new directory.
         for (QListIterator<Directory*> i(unwatchableEntriesCopy); i.hasNext();)
         {
            Directory* dir = i.next();
               this->scan(dir);
         }
      }

      if (this->toStop)
      {
         L_DEBU("FileUpdater mainloop finished");
         return;
      }
   }
}

/**
  * It will take some files from 'filesWithoutHashesPrioritized' or 'fileWithoutHashes' and compute theirs hashes.
  * The minimum duration of the compuation is equal to the setting 'minimum_duration_when_hashing'.
  */
void FileUpdater::computeSomeHashes()
{
   QMutexLocker locker(&this->hashingMutex);

   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      return;
   }

   if (this->filesWithoutHashes.isEmpty() && this->filesWithoutHashesPrioritized.isEmpty())
      return;

   L_DEBU("Start computing some hashes . . .");

   QElapsedTimer timer;
   timer.start();

   // We take the file from the prioritized list first.
   QList<QList<File*>*> fileLists { &this->filesWithoutHashesPrioritized, &this->filesWithoutHashes };
   for (QMutableListIterator<QList<File*>*> i(fileLists); i.hasNext();)
   {
      QList<File*>* fileList = i.next();
      while (!fileList->empty())
      {
         File* nextFileToHash = fileList->first();

         if (nextFileToHash->isComplete()) // A file can change its state from 'completed' to 'unfinished' if it's redownloaded.
         {
            locker.unlock();
            bool gotAllHashes;
            try
            {
               int hashedAmount = 0;
               gotAllHashes = this->fileHasher.start(nextFileToHash->asFileForHasher(), 1, &hashedAmount); // Be carreful of methods 'prioritizeAFileToHash(..)' and 'rmRoot(..)' called concurrently here.
               this->remainingSizeToHash -= hashedAmount;
               this->updateHashingProgress();
            }
            catch (IOErrorException&)
            {
               gotAllHashes = true; // The hashes may be recomputed when a peer ask the hashes with a GET_HASHES request.
            }
            locker.relock();

            // The current hashing file may have been removed from 'filesWithoutHashes' or 'filesWithoutHashesPrioritized' by 'rmRoot(..)'.
            if (gotAllHashes && !fileList->isEmpty() && fileList->first() == nextFileToHash)
               fileList->removeFirst();

            // Special case for the prioritized list, we put the file at the end after the computation of a hash.
            else if (fileList == &this->filesWithoutHashesPrioritized && fileList->size() > 1 && fileList->first() == nextFileToHash)
               fileList->move(0, fileList->size() - 1);
         }
         else
         {
            this->remainingSizeToHash -= fileList->first()->getSize();
            fileList->removeFirst();
         }

         if (this->toStopHashing)
         {
            this->toStopHashing = false;
            goto end;
         }

         static const quint32 MINIMUM_DURATION_WHEN_HASHING = SETTINGS.get<quint32>("minimum_duration_when_hashing");
         if (static_cast<quint32>(timer.elapsed()) >= MINIMUM_DURATION_WHEN_HASHING)
            goto end;
      }
   }

end:
   L_DEBU(QString("Computing some hashes ended. this->filesWithoutHashes.size(): %1, this->filesWithoutHashesPrioritized.size(): %2").arg(this->filesWithoutHashes.size()).arg(this->filesWithoutHashesPrioritized.size()));

   if (this->filesWithoutHashes.isEmpty() && this->filesWithoutHashesPrioritized.isEmpty())
   {
      this->remainingSizeToHash = 0;
      this->progress = 0;
   }
}

void FileUpdater::updateHashingProgress()
{
   const quint64 totalAmountOfData = this->fileManager->getAmount();
   QMutexLocker locker(&this->mutex);
   this->progress = totalAmountOfData == 0 ? 0 : 10000LL * (totalAmountOfData - this->remainingSizeToHash) / totalAmountOfData;
}

/**
  * Stop the current hashing process or the next hashing process.
  * The file is requeued.
  */
void FileUpdater::stopHashing()
{
   QMutexLocker lockerHashing(&this->hashingMutex);
   L_DEBU("Stop hashing . . .");

   this->fileHasher.stop();

   L_DEBU("Hashing stopped");
   this->toStopHashing = true;
}

/**
  * Synchronize the cache with the file system.
  * Scan recursively all the directories and files contained
  * in entry (if 'entry' is a directory). Create the associated cached tree structure under a
  * given 'Directory*'.
  * The directories may already exist in the cache.
  */
void FileUpdater::scan(Entry* entry, bool addUnfinished)
{
   L_DEBU("Start scanning a shared entry: " + entry->getFullPath());

   this->scanningMutex.lock();
   this->currentScanningEntry = entry;
   this->scanningMutex.unlock();

   QLinkedList<Directory*> dirsToVisit;
   dirsToVisit << dir;

   while (!dirsToVisit.isEmpty())
   {
      Directory* currentDir = dirsToVisit.takeFirst();

      QLinkedList<Directory*> currentSubDirs = currentDir->getSubDirs();
      QList<File*> currentFiles = currentDir->getCompleteFiles(); // We don't care about the unfinished files.

      foreach (QFileInfo fileInfo, QDir(currentDir->getFullPath()).entryInfoList(QDir::AllEntries | QDir::NoDotAndDotDot | QDir::NoSymLinks)) // TODO: Add an option to follow or not symlinks.
      {
         QMutexLocker locker(&this->scanningMutex);

         if (!this->currentScanningEntry || this->toStop)
         {
            L_DEBU("Scanning aborted: " + entry->getFullPath());
            this->currentScanningEntry = nullptr;
            this->scanningStopped.wakeOne();
            return;
         }

         if (fileInfo.isDir())
         {
            Directory* dir = currentDir->createSubDir(fileInfo.fileName());
            dir->setScanned(false);
            dirsToVisit << dir;

            currentSubDirs.removeOne(dir);
         }
         else if (addUnfinished || !Global::isFileUnfinished(fileInfo.fileName()))
         {
            File* file = currentDir->getFile(fileInfo.fileName());
            QMutexLocker locker(&this->mutex);

            // Only used when loading the cache to compute the progress.
            if (this->fileCacheInformation)
            {
               this->fileCacheInformation->newFile();
               this->progress = this->fileCacheInformation->getProgress();
            }

            if (file)
            {
               if (
                   !this->filesWithoutHashes.contains(file) && // The case where a file is being copied and a lot of modification event is thrown (thus the file is in this->filesWithoutHashes).
                   !this->filesWithoutHashesPrioritized.contains(file) &&
                   file->isComplete() &&
                   !file->correspondTo(fileInfo, file->hasAllHashes()) // If the hashes of a file can't be computed (IO error, the file is being written for example) we only compare their sizes.
               )
               {
                  currentFiles.removeOne(file);
                  this->deleteEntry(file);
                  file = nullptr;
               }
               else
               {
                  currentFiles.removeOne(file);
               }
            }

            if (!file)
            {
               // Very special case : there is a file 'a' without File* in cache and a file 'a.unfinished'.
               // This case occure when a file is redownloaded, the File* 'a' is renamed as 'a.unfinished' but the physical file 'a'
               // is not deleted.
               File* unfinishedFile = currentDir->getFile(fileInfo.fileName().append(Global::getUnfinishedSuffix()));
               if (!unfinishedFile)
                  file = new File(currentDir, fileInfo.fileName(), fileInfo.size(), fileInfo.lastModified());
               else
               {
                  currentFiles.removeOne(unfinishedFile);
                  continue;
               }
            }

            // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
            if (file->getSize() > 0 && !file->hasAllHashes() && file->isComplete() && !this->filesWithoutHashes.contains(file) && !this->filesWithoutHashesPrioritized.contains(file))
            {
               this->filesWithoutHashes << file;
               this->remainingSizeToHash += file->getSize();
            }
         }
      }

      // Deletes all the files and directories which doesn't exist on the file system.
      foreach (File* f, currentFiles)
         this->deleteEntry(f);

      foreach (Directory* d, currentSubDirs)
         this->deleteEntry(d);

      currentDir->setScanned(true);
   }

   this->scanningMutex.lock();
   this->currentScanningEntry = nullptr;
   this->scanningStopped.wakeOne();
   this->scanningMutex.unlock();

   this->mutex.lock();
   if (this->unwatchableEntries.contains(dir))
      this->timerScanUnwatchable.start();
   this->mutex.unlock();

   L_DEBU("Scanning terminated : " + dir->getFullPath());
}

/**
  * If you omit 'sharedEntry' then all scanning will be removed
  * from the queue.
  */
void FileUpdater::stopScanning(Entry* entry)
{
   QMutexLocker scanningLocker(&this->scanningMutex);
   if (!entry && this->currentScanningEntry || entry && this->currentScanningEntry == entry)
   {
      this->currentScanningEntry = nullptr;
      this->scanningStopped.wait(&this->scanningMutex);
   }
   else
   {
      QMutexLocker locker(&this->mutex);
      if (entry)
         this->entriesToScan.removeOne(entry);
      else
         this->entriesToScan.clear();
   }
}

/**
  * Delete an entry and if it's a directory remove it and its sub childs from 'this->dirsToScan'.
  * It can't be used to remove a 'SharedDirectory', only the 'Cache' is able to do that.
  */
void FileUpdater::deleteEntry(Entry* entry)
{
   if (!entry)
      return;

   QMutexLocker locker(&this->mutex);

   this->removeFromFilesWithoutHashes(entry);
   this->removeFromEntriesToScan(entry);

   entry->removeUnfinishedFiles();
   entry->del();
}

/**
  * Remove a directory and its sub directories from 'this->dirsToScan'.
  */
void FileUpdater::removeFromEntriesToScan(Entry* entry)
{
   this->entriesToScan.removeOne(entry);

   if (Directory* dir = dynamic_cast<Directory*>(entry))
   {
      DirIterator i(dir);
      while (Directory* subDir = i.next())
         this->entriesToScan.removeOne(subDir);
   }
}

/**
  * Remove all the pending files owned by 'dir'.
  */
void FileUpdater::removeFromFilesWithoutHashes(Entry* entry)
{
   if (Directory* dir = dynamic_cast<Directory*>(entry))
   {
      for (QMutableListIterator<File*> i(this->filesWithoutHashes); i.hasNext();)
      {
         File* f = i.next();
         if (f->hasAParentDir(dir))
         {
            this->remainingSizeToHash -= f->getSize();
            i.remove();
         }
      }

      for (QMutableListIterator<File*> i(this->filesWithoutHashesPrioritized); i.hasNext();)
      {
         File* f = i.next();
         if (f->hasAParentDir(dir))
         {
            this->remainingSizeToHash -= f->getSize();
            i.remove();
         }
      }
   }
   else if (File* file = dynamic_cast<File*>(entry))
   {
      bool fileInAList = this->filesWithoutHashes.removeOne(file);
      fileInAList |= this->filesWithoutHashesPrioritized.removeOne(file);

      if (fileInAList)
         this->remainingSizeToHash -= file->getSize();
   }
}

/**
  * Try to restore the chunk hashes from 'fileCache'.
  * 'fileCache' is set by 'retrieveFromFile(..)'.
  */
/*void FileUpdater::restoreFromFileCache(SharedDirectory* dir)
{
   //TODO

   L_DEBU("Start restoring hashes of a shared directory : " + dir->getFullPath());

   if (!this->fileCacheInformation)
   {
      L_ERRO("FileUpdater::restoreFromFileCache(..) : 'this->fileCache' must be previously set. Unable to restore from the file cache.");
      return;
   }

   for (int i = 0; i < this->fileCacheInformation->getFileCache()->shareddir_size(); i++)
      if (this->fileCacheInformation->getFileCache()->shareddir(i).id().hash() == dir->getId())
      {
         QSet<File*> filesWithHashes = dir->restoreFromFileCache(this->fileCacheInformation->getFileCache()->shareddir(i).root()).toSet();

         for (QMutableListIterator<File*> i(this->filesWithoutHashes); i.hasNext();)
         {
            File* f = i.next();
            if (filesWithHashes.contains(f))
            {
               this->remainingSizeToHash -= f->getSize();
               i.remove();
            }
         }

         break;
      }

   L_DEBU("Restoring terminated: " + dir->getFullPath());
   */
}

/**
  * Event from the filesystem like a new created file or a renamed file.
  * return true is at least one event is a timeout.
  */
bool FileUpdater::processEvents(const QList<WatcherEvent>& events)
{
   if (events.isEmpty())
      return false;

   foreach (WatcherEvent event, events)
   {
      if (event.type == WatcherEvent::TIMEOUT)
         return true;

      // Don't care about unfinished files.
      if (Global::isFileUnfinished(event.path1))
         continue;

      L_DEBU(QString("A file structure event occurs :\n%1").arg(event.toStr()));

      switch (event.type)
      {
      case WatcherEvent::MOVE:
         {
            const int lastSlashDestination = event.path2.lastIndexOf('/');
            if (lastSlashDestination == -1)
               break;

            const QString& destinationPath = event.path2.left(lastSlashDestination);
            Directory* destination = dynamic_cast<Directory*>(this->fileManager->getEntry(destinationPath));

            Entry* entryToMove = this->fileManager->getEntry(event.path1);

            if (entryToMove)
            {
               entryToMove->rename(event.path2.right(event.path2.size() - lastSlashDestination - 1));

               if (destination)
               {
                  entryToMove->moveInto(destination);
               }
               // A shared directory is moved in a directory not in cache.
               else if (SharedDirectory* sharedToMove = dynamic_cast<SharedDirectory*>(entryToMove))
               {
                  sharedToMove->moveInto(destinationPath);
               }
               else
               {
                  this->deleteEntry(entryToMove);
               }
            }

            break;
         }

      case WatcherEvent::DELETED:
         {
            SharedEntry* sharedEntry = this->fileManager->getSharedEntry(event.path1);
            if (sharedEntry)
               emit deleteSharedEntry(sharedEntry);
            else
            {
               Entry* entry = this->fileManager->getEntry(event.path1);
               if (entry)
                  this->deleteEntry(entry);
            }
            break;
         }

      case WatcherEvent::NEW:
      case WatcherEvent::CONTENT_CHANGED:
         {
            Directory* dir = this->fileManager->getFittestDirectory(event.path1);
            if (dir && !this->dirsToScan.contains(dir))
               this->dirsToScan << dir;
            break;
         }

      case WatcherEvent::UNKNOWN:
      case WatcherEvent::TIMEOUT:
         break; // Do nothing.
      }
   }

   return false;
}

/////

FileUpdater::FileCacheInformation::FileCacheInformation(const Protos::FileCache::Hashes* fileCache) :
   fileCache(fileCache), fileCacheNbFiles(0), fileCacheNbFilesLoaded(0)
{
   for (int i = 0; i < this->fileCache->shareddir_size(); i++)
      this->computeFileCacheNbFiles(this->fileCache->shareddir(i).root());
}

FileUpdater::FileCacheInformation::~FileCacheInformation()
{
   delete this->fileCache;
}

void FileUpdater::FileCacheInformation::newFile()
{
   if (this->fileCacheNbFilesLoaded == this->fileCacheNbFiles)
      return;
   this->fileCacheNbFilesLoaded++;
}

const Protos::FileCache::Hashes* FileUpdater::FileCacheInformation::getFileCache()
{
   return this->fileCache;
}

/**
  * return a value between 0 and 10000 (basis point).
  */
int FileUpdater::FileCacheInformation::getProgress() const
{
   if (this->fileCacheNbFiles == 0)
      return 0;
   return 10000LL * this->fileCacheNbFilesLoaded / this->fileCacheNbFiles;
}

void FileUpdater::FileCacheInformation::computeFileCacheNbFiles(const Protos::FileCache::Hashes::Dir& dir)
{
   for (int i = 0; i < dir.dir_size(); i++)
      this->computeFileCacheNbFiles(dir.dir(i));

   this->fileCacheNbFiles += dir.file_size();
}
