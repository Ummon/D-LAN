/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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

#include <priv/Global.h>
#include <priv/Exceptions.h>
#include <priv/Log.h>
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

FileUpdater::FileUpdater(FileManager* fileManager) :
   SCAN_PERIOD_UNWATCHABLE_DIRS(SETTINGS.get<quint32>("scan_period_unwatchable_dirs")),
   fileManager(fileManager),
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

   this->wait();
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

   this->dirsToScan << dir;

   if (!watchable)
   {
      L_WARN(QString("This directory is not watchable : %1").arg(dir->getFullPath()));
      this->unwatchableDirs << dir;
   }

   this->dirEvent->release();
}

/**
  * Called by another thread.
  * If 'dir2' is given it will steal the subdirs of 'dir' and append
  * them to itself.
  */
void FileUpdater::rmRoot(SharedDirectory* dir, Directory* dir2)
{
   // If there is a scanning for this directory stop it.
   this->stopScanning(dir);

   QMutexLocker locker(&this->mutex);

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
      this->unwatchableDirs.removeOne(dir);
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
   L_DEBU(QString("FileUpdater::prioritizeAFileToHash : %1").arg(file->getFullPath()));

   // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
   if (!file->hasAllHashes() && file->isComplete())
   {
      QMutexLocker lockerHashing(&this->hashingMutex);

      this->filesWithoutHashes.removeOne(file);
      if (!this->filesWithoutHashesPrioritized.contains(file))
         this->filesWithoutHashesPrioritized.prepend(file);

      if (this->currentHashingFile)
      {
         this->currentHashingFile->stopHashing();
         this->toStopHashing = true;
      }
   }
}

bool FileUpdater::isScanning() const
{
   QMutexLocker scanningLocker(&this->scanningMutex);
   return this->currentScanningDir != 0;
}

bool FileUpdater::isHashing() const
{
   QMutexLocker locker(&this->mutex);
   return !this->filesWithoutHashes.isEmpty() || !this->filesWithoutHashesPrioritized.isEmpty();
}

void FileUpdater::run()
{
   this->timerScanUnwatchable.start();

   QString threadName = "FileUpdater";
#if DEBUG
   threadName.append("_").append(QString::number((quint32)QThread::currentThreadId()));
#endif
   QThread::currentThread()->setObjectName(threadName);

   // First : retrieve the directories and file from the file cache and
   // synchronize it with the file system.
   if (this->fileCache)
   {
      // TODO : the mutex should be used ?
      foreach (Directory* dir, this->dirsToScan)
      {
         this->scan(dir, true);
         this->restoreFromFileCache(static_cast<SharedDirectory*>(dir));
      }
      this->dirsToScan.clear();

      delete this->fileCache;
      this->fileCache = 0;
   }

   emit fileCacheLoaded();

   forever
   {
      this->computeSomeHashes();

      this->mutex.lock();

      foreach (SharedDirectory* dir, this->dirsToRemove)
      {
         L_DEBU(QString("Stop watching this directory : %1").arg(dir->getFullPath()));
         if (this->dirWatcher)
            this->dirWatcher->rmDir(dir->getFullPath());

         dir->removeUnfinishedFiles();
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
            this->dirEvent->wait(this->unwatchableDirs.isEmpty() ? -1 : SCAN_PERIOD_UNWATCHABLE_DIRS);
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
            this->treatEvents(this->dirWatcher->waitEvent(this->unwatchableDirs.isEmpty() ? -1 : SCAN_PERIOD_UNWATCHABLE_DIRS, QList<WaitCondition*>() << this->dirEvent));
         }
         else
         {
            this->mutex.unlock();
            this->treatEvents(this->dirWatcher->waitEvent(0)); // Just pick the new events. (Don't wait for new event).
         }
      }

      if (timerScanUnwatchable.elapsed() >= SCAN_PERIOD_UNWATCHABLE_DIRS)
      {
         this->mutex.lock();
         QList<Directory*> unwatchableDirsCopy = this->unwatchableDirs;
         this->mutex.unlock();

         // Synchronize the new directory.
         for (QListIterator<Directory*> i(unwatchableDirsCopy); i.hasNext();)
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
  * It will take some files from 'fileWithoutHashes' and compute theirs hashes.
  * The duration of the compuation is minimum 'minimumDurationWhenHashing'.
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

   L_DEBU("Start computing some hashes..");

   QElapsedTimer timer;
   timer.start();

   for (int i = 0; i < this->filesWithoutHashesPrioritized.size(); i++)
   {
      this->currentHashingFile = this->filesWithoutHashesPrioritized.first();

      if (this->currentHashingFile->isComplete()) // A file can change its state from 'completed' to 'unfinished' if it's redownloaded.
      {
         locker.unlock();
         bool complete = this->currentHashingFile->computeHashes(1);
         locker.relock();
         if (complete)
         {
            this->filesWithoutHashesPrioritized.removeFirst();
            i--;
         }
         else if (!this->filesWithoutHashesPrioritized.isEmpty()) // The current hashing file may have been removed from 'filesWithoutHashesPrioritized' by 'rmRoot(..)'.
            this->filesWithoutHashesPrioritized.move(0, this->filesWithoutHashesPrioritized.size() - 1);
      }
      else
      {
         this->filesWithoutHashesPrioritized.removeFirst();
         i--;
      }

      this->currentHashingFile = 0;
      if (this->toStopHashing)
      {
         this->toStopHashing = false;
         goto end;
      }

      if (static_cast<quint32>(timer.elapsed()) >= SETTINGS.get<quint32>("minimum_duration_when_hashing"))
         goto end;
   }

   for (int i = 0; i < this->filesWithoutHashes.size(); i++)
   {
      this->currentHashingFile = this->filesWithoutHashes[i];

       // A file can change its state from 'completed' to 'unfinished' if it's redownloaded.
      if (this->currentHashingFile->isComplete())
      {
         locker.unlock();
         bool complete = this->currentHashingFile->computeHashes();
         locker.relock();
         if (complete)
            this->filesWithoutHashes.removeAt(i--);
      }
      else
         this->filesWithoutHashes.removeAt(i--);

      this->currentHashingFile = 0;
      if (this->toStopHashing)
      {
         this->toStopHashing = false;
         goto end;
      }

      if (static_cast<quint32>(timer.elapsed()) >= SETTINGS.get<quint32>("minimum_duration_when_hashing"))
         goto end;
   }

end:
   L_DEBU("Computing some hashes ended");
}

/**
  * Stop the current hashing process or the next hashing process.
  * The file is requeued.
  */
void FileUpdater::stopHashing()
{
   QMutexLocker lockerHashing(&this->hashingMutex);
   L_DEBU("Stop hashing...");

   if (this->currentHashingFile)
      this->currentHashingFile->stopHashing();

   L_DEBU("Hashing stopped");
   this->toStopHashing = true;
}

/**
  * Synchronize the cache with the file system.
  * Scan recursively all the directories and files contained
  * in dir. Create the associated cached tree structure under the
  * given 'Directory*'.
  * The directories may already exist in the cache.
  */
void FileUpdater::scan(Directory* dir, bool addUnfinished)
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

      foreach (QFileInfo entry, QDir(currentDir->getFullPath()).entryInfoList(QDir::AllEntries | QDir::NoDotAndDotDot | QDir::NoSymLinks))
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
         else if (entry.size() > 0 && (addUnfinished || !Global::isFileUnfinished(entry.fileName())))
         {
            File* file = currentDir->getFile(entry.fileName());
            QMutexLocker locker(&this->mutex);

            if (file)
            {
               if (
                   !this->filesWithoutHashes.contains(file) && // The case where a file is being copied and a lot of modification event is thrown (thus the file is in this->filesWithoutHashes).
                   !this->filesWithoutHashesPrioritized.contains(file) &&
                   file->isComplete() &&
                   !file->correspondTo(entry)
               )
                  file = 0;
               else
                  currentFiles.removeOne(file);
            }

            if (!file)
            {
               // Very special case : there is a file 'a' without File* in cache and a file 'a.unfinished'.
               // This case occure when a file is redownloaded, the File* 'a' is renamed as 'a.unfinished' but the physical file 'a'
               // is not deleted.
               File* unfinishedFile;
               if (!(unfinishedFile = currentDir->getFile(entry.fileName().append(Global::getUnfinishedSuffix()))))
                  file = new File(currentDir, entry.fileName(), entry.size(), entry.lastModified());
               else
               {
                  currentFiles.removeOne(unfinishedFile);
                  continue;
               }
            }

            // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
            if (!file->hasAllHashes() && file->isComplete() && !this->filesWithoutHashes.contains(file) && !this->filesWithoutHashesPrioritized.contains(file))
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

   this->mutex.lock();
   if (this->unwatchableDirs.contains(dir))
      this->timerScanUnwatchable.start();
   this->mutex.unlock();

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
      this->filesWithoutHashesPrioritized.removeOne(file);
      file->removeUnfinishedFiles();
      delete file;
   }
   else
   {
      entry->removeUnfinishedFiles();
      delete entry;
   }
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

   for (QMutableListIterator<File*> i(this->filesWithoutHashesPrioritized); i.hasNext();)
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
  * return true is at least one event is a timeout.
  */
bool FileUpdater::treatEvents(const QList<WatcherEvent>& events)
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
            // TODO : move the entry if needed. (create a method in Cache)
            // TODO : update the file modification date (only if the rename under Windows changes it)
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

      case WatcherEvent::CONTENT_CHANGED:
         {
            Directory* dir = this->fileManager->getFittestDirectory(event.path1);
            if (dir && !this->dirsToScan.contains(dir))
               this->dirsToScan << dir;
            break;
         }

      case WatcherEvent::NEW:
      case WatcherEvent::UNKNOWN:
      case WatcherEvent::TIMEOUT:
         break; // Do nothing.
      }
   }

   return false;
}
