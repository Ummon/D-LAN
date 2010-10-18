#include <priv/FileUpdater/FileUpdater.h>
using namespace FM;

#include <QLinkedList>
#include <QDir>
#include <QElapsedTimer>

#include <Common/Settings.h>

#include <priv/Log.h>
#include <priv/Exceptions.h>
#include <priv/Constants.h>
#include <priv/FileManager.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>
#include <priv/FileUpdater/WaitCondition.h>

/**
  * @class FileUpdater
  *
  */

FileUpdater::FileUpdater(FileManager* fileManager)
   : fileManager(fileManager),
     dirWatcher(DirWatcher::getNewWatcher()),
     fileCache(0),
     toStop(false),
     mutex(QMutex::Recursive),
     currentScanningDir(0),
     currentHashingFile(0),
     toStopHashing(false)
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

   L_DEBU("OK4");
   this->wait();

   L_DEBU("OK5");
}

/**
  * Called by another thread.
  * @exception DirNotFoundException
  */
void FileUpdater::addRoot(SharedDirectory* dir)
{
   QMutexLocker locker(&this->mutex);

   bool watchable = false;
   if (this->dirWatcher)
      watchable = this->dirWatcher->addDir(dir->getFullPath());

   // TODO : treat the unwatchable directory
   if (!watchable)
      L_WARN(QString("This directory is not watchable : %1").arg(dir->getFullPath()));

   this->dirsToScan << dir;

   this->dirEvent->release();
}

/**
  * Called by another thread.
  * If 'dir2' is given it will steal the subdirs of 'dir' and append
  * them to itself.
  */
void FileUpdater::rmRoot(SharedDirectory* dir, Directory* dir2)
{
   QMutexLocker locker(&this->mutex);

   // If there is a scanning for this directory stop it.
   this->stopScanning(dir);

   // Stop the hashing to modify 'this->fileWithoutHashes'.
   // TODO : A suspend/resume hashing methods would be more readable.
   {
      QMutexLocker locker(&this->hashingMutex);

      if (this->currentHashingFile)
         this->currentHashingFile->stopHashing();
      this->toStopHashing = true;

      // TODO : Find a more elegant way!
      if (dir2)
         dir2->stealContent(dir);

      this->removeFromFilesWithoutHashes(dir);
      this->removeFromDirsToScan(dir);
      this->dirsToRemove << dir;
   }

   this->dirEvent->release();
}

/**
  * Set the file cache to retrieve the hashes frome it.
  * Muste be called before starting the fileUpdater.
  * The shared dirs in fileCache must be previously added by 'addRoot(..)'.
  * This object must unallocated the hashes.
  */
void FileUpdater::setFileCache(const Protos::FileCache::Hashes* fileCache)
{
   this->fileCache = fileCache;
}

void FileUpdater::prioritizeAFileToHash(File* file)
{
   QMutexLocker locker(&this->mutex);

   // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
   if (!file->hasAllHashes() && file->isComplete())
   {
      QMutexLocker lockerHashing(&this->hashingMutex);
      this->filesWithoutHashes.removeOne(file);
      this->filesWithoutHashes.prepend(file);
   }

   this->stopHashing();
}

void FileUpdater::run()
{
   QThread::currentThread()->setObjectName("FileUpdater");

   // First : retrieve the directories and file from the file cache and
   // synchronize it with the file system.
   if (this->fileCache)
   {
      // TODO : the mutex should be used ?
      foreach (Directory* dir, this->dirsToScan)
      {
         this->scan(dir);
         this->restoreFromFileCache(static_cast<SharedDirectory*>(dir));
      }
      this->dirsToScan.clear();

      delete this->fileCache;
      this->fileCache = 0;
   }

   emit fileCacheLoaded();

   forever
   {
      if (this->computeSomeHashes())
         emit persistCache();

      this->mutex.lock();

      foreach (SharedDirectory* dir, this->dirsToRemove)
      {
         L_DEBU(QString("Stop watching this directory : %1").arg(dir->getFullPath()));
         if (this->dirWatcher)
            this->dirWatcher->rmDir(dir->getFullPath());

         delete dir;
      }
      this->dirsToRemove.clear();

      // If there is no watcher capability or no directory to watch then
      // we wait for an added directory.
      if (!this->dirWatcher || this->dirWatcher->nbWatchedDir() == 0 || !this->dirsToScan.empty())
      {
         if (this->dirsToScan.isEmpty())
         {
            L_DEBU("Waiting for a new shared directory added..");
            this->mutex.unlock();
            this->dirEvent->wait();
         }
         else
            this->mutex.unlock();

         Directory* addedDir = 0;
         this->mutex.lock();
         if (!this->dirsToScan.isEmpty())
            addedDir = this->dirsToScan.takeLast();
         this->mutex.unlock();

         // Synchronize the new directory.
         if (addedDir)
            this->scan(addedDir);
      }
      else // Wait for filesystem modifications.
      {
         // If we have no dir to scan and no file to hash we wait for a new shared file
         // or a filesystem event.
         if (this->dirsToScan.isEmpty() && this->filesWithoutHashes.isEmpty())
         {
            this->mutex.unlock();
            this->treatEvents(this->dirWatcher->waitEvent(QList<WaitCondition*>() << this->dirEvent));
         }
         else
         {
            this->mutex.unlock();
            this->treatEvents(this->dirWatcher->waitEvent(0));
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
  * It will take some file from 'fileWithoutHashes' and compute theirs hashes.
  * The duration of the compuation is minimum 'minimumDurationWhenHashing'.
  * @return true if some file has been hashed
  */
bool FileUpdater::computeSomeHashes()
{
   this->hashingMutex.lock();
   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      L_DEBU("Computing some hashes aborted");
      this->hashingMutex.unlock();
      return false;
   }

   if (this->filesWithoutHashes.isEmpty())
   {
      this->hashingMutex.unlock();
      return false;
   }

   L_DEBU("Start computing some hashes..");

   QElapsedTimer timer;
   timer.start();

   for (QMutableListIterator<File*> i(this->filesWithoutHashes); i.hasNext();)
   {
      this->currentHashingFile = i.next();

      this->hashingMutex.unlock();
      bool completed = this->currentHashingFile->computeHashes();
      this->hashingMutex.lock();

      this->currentHashingFile = 0;
      if (this->toStopHashing)
      {
         this->toStopHashing = false;
         L_DEBU("Computing some hashes aborted");
         this->hashingMutex.unlock();
         return false;
      }

      if (completed)
         i.remove();

      if (static_cast<quint32>(timer.elapsed()) >= SETTINGS.get<quint32>("minimum_duration_when_hashing"))
         break;
   }

   L_DEBU("Computing some hashes terminated");

   this->hashingMutex.unlock();
   return true;
}

/**
  * Stop the current hashing process or the next hashing process.
  * The file is requeued.
  */
void FileUpdater::stopHashing()
{
   QMutexLocker lockerHashing(&this->hashingMutex);
   if (this->currentHashingFile)
   {
      this->currentHashingFile->stopHashing();
      QMutexLocker locker(&this->mutex);
      this->filesWithoutHashes.append(this->currentHashingFile);
   }

   this->toStopHashing = true;
}

/**
  * Synchronize the cache with the file system.
  * Scan recursively all the directories and files contained
  * in dir. Create the associated cached tree structure under the
  * given 'Directory*'.
  * The directories may already exist in the cache.
  */
void FileUpdater::scan(Directory* dir)
{
   L_DEBU("Start scanning a shared directory : " + dir->getFullPath());

   this->scanningMutex.lock();
   this->currentScanningDir = dir;
   this->scanningMutex.unlock();

   QLinkedList<Directory*> dirsToVisit;
   dirsToVisit << dir;

   while (!dirsToVisit.isEmpty())
   {
      Directory* currentDir = dirsToVisit.takeFirst();

      QList<Directory*> currentSubDirs = currentDir->getSubDirs();
      QList<File*> currentFiles = currentDir->getCompleteFiles(); // We don't care about the unfinished files.

      foreach (QFileInfo entry, QDir(currentDir->getFullPath()).entryInfoList(QDir::AllEntries | QDir::NoDotAndDotDot))
      {
         QMutexLocker locker(&this->scanningMutex);

         if (!this->currentScanningDir || this->toStop)
         {
            L_DEBU("Scanning aborted : " + dir->getFullPath());
            this->currentScanningDir = 0;
            this->scanningStopped.wakeOne();
            return;
         }

         if (entry.isDir())
         {
            Directory* dir = currentDir->createSubDirectory(entry.fileName());
            dirsToVisit << dir;

            currentSubDirs.removeOne(dir);
         }
         else if (entry.size() > 0 && !Cache::isFileUnfinished(entry.fileName()))
         {
            File* file = currentDir->getFile(entry.fileName());
            QMutexLocker locker(&this->mutex);

            if (file)
            {
               if (
                   !this->filesWithoutHashes.contains(file) && // The case where a file is being copied and a lot of modification event is thrown (thus the file is in this->filesWithoutHashes).
                   file->isComplete() &&
                   !file->correspondTo(entry)
               )
                  file = 0;
               else
                  currentFiles.removeOne(file);
            }

            if (!file)
               file = new File(currentDir, entry.fileName(), entry.size(), entry.lastModified());

            // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
            if (!file->hasAllHashes() && file->isComplete() && !this->filesWithoutHashes.contains(file))
            {
               this->filesWithoutHashes << file;
            }
         }
      }

      // Deletes all the files and directories which doesn't exist on the file system.
      foreach (File* f, currentFiles)
         this->deleteEntry(f);

      foreach (Directory* d, currentSubDirs)
         this->deleteEntry(d);
   }

   this->scanningMutex.lock();
   this->currentScanningDir = 0;
   this->scanningStopped.wakeOne();
   this->scanningMutex.unlock();

   L_DEBU("Scanning terminated : " + dir->getFullPath());
}

/**
  * If you omit 'dir' then all scanning will be removed
  * from the queue.
  */
void FileUpdater::stopScanning(Directory* dir)
{
   QMutexLocker scanningLocker(&this->scanningMutex);
   if (!dir && this->currentScanningDir || dir && this->currentScanningDir == dir)
   {
      this->currentScanningDir = 0;
      this->scanningStopped.wait(&this->scanningMutex);
   }
   else
   {
      QMutexLocker scanningLocker(&this->mutex);
      if (dir)
         this->dirsToScan.removeOne(dir);
      else
         this->dirsToScan.clear();
   }
}


/**
  * Delete an entry and if it's a directory remove it and its sub childs from dirsToScan.
  */
void FileUpdater::deleteEntry(Entry* entry)
{
   if (!entry)
      return;

   QMutexLocker locker(&this->mutex);

   // Remove the directory and their sub child from the scan list (this->dirsToScan).
   if (Directory* dir = dynamic_cast<Directory*>(entry))
   {
      this->removeFromFilesWithoutHashes(dir);
      this->removeFromDirsToScan(dir);
   }

   if (SharedDirectory* sharedDir = dynamic_cast<SharedDirectory*>(entry))
   {
      if (!this->dirsToRemove.contains(sharedDir))
         this->dirsToRemove << sharedDir;
   }
   else if (File* file = dynamic_cast<File*>(entry))
   {
      this->filesWithoutHashes.removeOne(file);
      delete file;
   }
   else
      delete entry;
}

/**
  * Remove a directory and its sub directories from 'this->dirsToScan'.
  */
void FileUpdater::removeFromDirsToScan(Directory* dir)
{
   this->dirsToScan.removeOne(dir);
   DirIterator i(dir);
   while (Directory* subDir = i.next())
      this->dirsToScan.removeOne(subDir);
}

/**
  * Remove all the pending files owned by 'dir'.
  */
void FileUpdater::removeFromFilesWithoutHashes(Directory* dir)
{
   for (QMutableListIterator<File*> i(this->filesWithoutHashes); i.hasNext();)
      if (i.next()->hasAParentDir(dir))
         i.remove();
}

/**
  * Try to restore the chunk hashes from 'fileCache'.
  * 'fileCache' is set by 'retrieveFromFile(..)'.
  */
void FileUpdater::restoreFromFileCache(SharedDirectory* dir)
{
   L_DEBU("Start restoring hashes of a shared directory : " + dir->getFullPath());

   if (this->fileCache == 0)
   {
      L_ERRO("FileUpdater::restoreFromFileCache : this->fileCache must be previously set. Unable to restore from the file cache.");
      return;
   }

   QList<File*> filesWithHashes = dir->restoreFromFileCache(*this->fileCache);

   // Remove the files which have a hash.
   // TODO : O(n^2) can be a bit long...
   for (QListIterator<File*>i(filesWithHashes); i.hasNext();)
      this->filesWithoutHashes.removeOne(i.next());

   L_DEBU("Restoring terminated : " + dir->getFullPath());
}

/**
  * Event from the filesystem like a new created file or a renamed file.
  */
void FileUpdater::treatEvents(const QList<WatcherEvent>& events)
{
   if (events.isEmpty())
      return;

   foreach (WatcherEvent event, events)
   {
      if (
         event.type == WatcherEvent::TIMEOUT || // Don't care about this event type.
         Cache::isFileUnfinished(event.path1) // Don't care about unfinished files.
      )
         continue;

      L_DEBU(QString("A file structure event occurs :\n%1").arg(event.toStr()));

      switch (event.type)
      {
      case WatcherEvent::RENAME:
         {
            Entry* entry = this->fileManager->getEntry(event.path1);
            if (entry)
               entry->changeName(event.path2.split('/', QString::SkipEmptyParts).last());
            break;
         }

      case WatcherEvent::DELETED:
         {
            Entry* entry = this->fileManager->getEntry(event.path1);
            this->deleteEntry(entry);
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
}
