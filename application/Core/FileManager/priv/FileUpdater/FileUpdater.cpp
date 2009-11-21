#include <priv/FileUpdater/FileUpdater.h>
using namespace FM;

#include <QLinkedList>
#include <QDir>
#include <QTime>

#include <priv/Log.h>
#include <priv/Exceptions.h>
#include <priv/Constants.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>
#include <priv/FileUpdater/WaitCondition.h>

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

   LOG_DEBUG("FileUpdater deleted");
}

void FileUpdater::stop()
{
   LOG_DEBUG("Stopping FileUpdater..");

   this->toStop = true;

   LOG_DEBUG("OK1");
   this->stopHashing();

   LOG_DEBUG("OK2");
   this->stopScanning();

   LOG_DEBUG("OK3");
   // Simulate a dirEvent to stop the main loop.
   this->dirEvent->release();

   LOG_DEBUG("OK4");
   this->wait();

   LOG_DEBUG("OK5");
}

void FileUpdater::addRoot(SharedDirectory* dir)
{
   QMutexLocker locker(&this->mutex);

   bool watchable = false;
   if (this->dirWatcher)
      watchable = this->dirWatcher->addDir(dir->getFullPath());

   // TODO : treat the unwatchable directory

   this->dirsToScan << dir;

   this->dirEvent->release();
}

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

      for (QMutableListIterator<File*> i(this->fileWithoutHashes); i.hasNext();)
         if (i.next()->getRoot() == dir)
            i.remove();
   }

   this->dirsToRemove << dir;

   this->dirEvent->release();
}

void FileUpdater::setFileCache(const Protos::FileCache::Hashes* fileCache)
{
   this->fileCache = fileCache;
}

void FileUpdater::run()
{
   QThread::currentThread()->setObjectName("FileUpdater");

   // First : retrieve the directories and file from the file cache and
   // synchronize it with the file system.
   if (this->fileCache)
   {
      // TODO : the mutex should be used ?
      for (QListIterator<SharedDirectory*> i(this->dirsToScan); i.hasNext();)
      {
         SharedDirectory* dir = i.next();
         this->scan(dir);
         this->restoreFromFileCache(dir);
      }
      this->dirsToScan.clear();

      delete this->fileCache;
      this->fileCache = 0;
   }

   forever
   {
      if (this->computeSomeHashes())
         emit persistCache();

      this->mutex.lock();

      foreach (SharedDirectory* dir, this->dirsToRemove)
      {
         LOG_DEBUG(QString("Stop watching this directory : %1").arg(dir->getFullPath()));
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
            LOG_DEBUG("Waiting for a new shared directory added..");
            this->mutex.unlock();
            this->dirEvent->wait();
         }
         else
            this->mutex.unlock();

         SharedDirectory* addedDir = 0;
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
         if (this->dirsToScan.isEmpty() && this->fileWithoutHashes.isEmpty())
         {
            this->mutex.unlock();
            this->treatEvents(this->dirWatcher->waitEvent(
                  QList<WaitCondition*>() << this->dirEvent)
            );
         }
         else
         {
            this->mutex.unlock();
            this->treatEvents(this->dirWatcher->waitEvent(0));
         }
      }
      if (this->toStop)
         return;
   }
}

bool FileUpdater::computeSomeHashes()
{
   this->hashingMutex.lock();
   if (this->toStopHashing)
   {
      this->toStopHashing = false;
      LOG_DEBUG("Computing some hashes aborted");
      this->hashingMutex.unlock();
      return false;
   }

   if (fileWithoutHashes.isEmpty())
   {
      this->hashingMutex.unlock();
      return false;
   }

   LOG_DEBUG("Start computing some hashes..");

   QTime time;
   time.start();

   QMutableListIterator<File*> i(this->fileWithoutHashes);
   while (i.hasNext())
   {
      this->currentHashingFile = i.next();

      this->hashingMutex.unlock();
      bool completed = this->currentHashingFile->computeHashes();
      this->hashingMutex.lock();

      this->currentHashingFile = 0;
      if (this->toStopHashing)
      {
         this->toStopHashing = false;
         LOG_DEBUG("Computing some hashes aborted");
         this->hashingMutex.unlock();
         return false;
      }

      if (completed)
         i.remove();

      if (time.elapsed() / 1000 >= MINIMUM_DURATION_WHEN_HASHING)
         break;
   }

   LOG_DEBUG("Computing some hashes terminated");

   this->hashingMutex.unlock();
   return true;
}

void FileUpdater::stopHashing()
{
   QMutexLocker locker(&this->hashingMutex);
   if (this->currentHashingFile)
      this->currentHashingFile->stopHashing();
   this->toStopHashing = true;
}

void FileUpdater::scan(SharedDirectory* sharedDir)
{
   LOG_DEBUG("Start scanning a shared directory : " + sharedDir->getFullPath());

   this->scanningMutex.lock();
   this->currentScanningDir = sharedDir;
   this->scanningMutex.unlock();

   QLinkedList<Directory*> dirsToVisit;
   dirsToVisit << sharedDir;

   while (!dirsToVisit.isEmpty())
   {
      Directory* currentDir = dirsToVisit.takeFirst();

      QList<Directory*> currentSubDirs = currentDir->getSubDirs();
      QList<File*> currentFiles = currentDir->getFiles();

      foreach (QFileInfo entry, QDir(currentDir->getFullPath()).entryInfoList())
      {
         if (entry.fileName() == "." || entry.fileName() == "..")
            continue;

         {
            QMutexLocker locker(&this->scanningMutex);

            if (!this->currentScanningDir || this->toStop)
            {
               LOG_DEBUG("Scanning aborted : " + sharedDir->getFullPath());
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
            else
            {
               File* file = currentDir->createFile(entry);

               // If a file is incomplete (unfinished) we can't compute its hashes because we don't have all data.
               if (!file->hasAllHashes() && file->isComplete())
                  this->fileWithoutHashes << file;

               currentFiles.removeOne(file);
            }
         }
      }

      // Deletes all the files and directories which doesn't exist on the file system.
      foreach (File* f, currentFiles)
         delete f;
      foreach (Directory* d, currentSubDirs)
         delete d;
   }

   this->scanningMutex.lock();
   this->currentScanningDir = 0;
   this->scanningStopped.wakeOne();
   this->scanningMutex.unlock();

   LOG_DEBUG("Scanning terminated : " + sharedDir->getFullPath());
}

void FileUpdater::stopScanning(SharedDirectory* dir)
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

void FileUpdater::restoreFromFileCache(SharedDirectory* dir)
{
   LOG_DEBUG("Start restoring hashes of a shared directory : " + dir->getFullPath());

   if (this->fileCache == 0)
   {
      LOG_ERR("FileUpdater::restoreFromFileCache : this->fileCache must be previously set. Unable to restore from the file cache.");
      return;
   }

   QList<File*> filesWithHashes = dir->restoreFromFileCache(*this->fileCache);

   // Remove the files which have a hash.
   // TODO : O(n^2) can be a bit long...
   for (QListIterator<File*>i(filesWithHashes); i.hasNext();)
      this->fileWithoutHashes.removeOne(i.next());

   LOG_DEBUG("Restoring terminated : " + dir->getFullPath());
}

void FileUpdater::treatEvents(const QList<WatcherEvent>& events)
{
   if (events.isEmpty())
      return;

   foreach (WatcherEvent event, events)
   {
      if (event.type == WatcherEvent::TIMEOUT)
         continue;

      // TODO..
      LOG_DEBUG("File structure event occurs :");
      LOG_DEBUG(event.toStr());
   }
}
