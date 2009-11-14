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
     currentScanningDir(0),
     currentHashingFile(0),
     toStopHashing(false)
{
   this->dirEvent = WaitCondition::getNewWaitCondition();
   this->stopEvent = WaitCondition::getNewWaitCondition();
}

FileUpdater::~FileUpdater()
{
   if (this->dirEvent)
      delete this->dirEvent;
   if (this->stopEvent)
      delete this->stopEvent;

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
   this->stopEvent->release();

   LOG_DEBUG("OK4");
   this->wait();

   LOG_DEBUG("OK5");
}

void FileUpdater::addRoot(SharedDirectory* dir)
{
   QMutexLocker(&this->mutex);

   if (this->dirWatcher)
      this->dirWatcher->addDir(dir->getFullPath());

   this->dirsToScan << dir;

   this->dirEvent->release();
}

void FileUpdater::rmRoot(SharedDirectory* dir)
{
   QMutexLocker(&this->mutex);

   // If there is a scanning for this directory stop it.
   this->stopScanning(dir);

   this->stopHashing();
   for (QMutableListIterator<File*> i(this->fileWithoutHashes); i.hasNext();)
      if (i.next()->getRoot() == dir)
         i.remove();

   this->dirsToRemove << dir->getFullPath();

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

      if (!this->dirsToRemove.isEmpty())
      {
         foreach (QString path, this->dirsToRemove)
         {
            LOG_DEBUG(QString("Stop watching this directory : %1").arg(path));
            if (this->dirWatcher)
               this->dirWatcher->rmDir(path);
         }
         this->dirsToRemove.clear();
      }

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
            this->treatEvents(this->dirWatcher->waitEvent(QList<WaitCondition*>() << this->dirEvent << this->stopEvent));
         }
         else
         {
            this->mutex.unlock();
            this->treatEvents(this->dirWatcher->waitEvent(0));
         }
         if (this->toStop)
            return;
      }
   }
}

bool FileUpdater::computeSomeHashes()
{
   if (fileWithoutHashes.isEmpty())
      return false;

   LOG_DEBUG("Start computing some hashes..");

   QTime time;
   time.start();

   QMutableListIterator<File*> i(this->fileWithoutHashes);
   while (i.hasNext())
   {
      {
         QMutexLocker(&this->hashingMutex);
         if (this->toStop)
            return false;
         this->currentHashingFile = i.next();
      }
         bool completed = this->currentHashingFile->computeHashes();
      {
         QMutexLocker(&this->hashingMutex);
         this->currentHashingFile = 0;
         if (this->toStopHashing)
         {
            this->toStopHashing = false;
            LOG_DEBUG("Computing some hashes aborted");
            return false;
         }
      }

      if (completed)
         i.remove();

      if (time.elapsed() / 1000 >= MINIMUM_DURATION_WHEN_HASHING)
         break;
   }

   LOG_DEBUG("Computing some hashes terminated");

   return true;
}

void FileUpdater::stopHashing()
{
   QMutexLocker(&this->hashingMutex);
   if (this->currentHashingFile)
   {
      this->toStopHashing = true;
      this->currentHashingFile->stopHashing();
   }
}

void FileUpdater::scan(SharedDirectory* sharedDir)
{
   LOG_DEBUG("Start scanning a shared directory : " + sharedDir->getFullPath());

   if (sharedDir->isDeleted())
   {
      LOG_DEBUG("Cannot scan a deleted shared directory : " + sharedDir->getFullPath());
      return;
   }

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
               if (!file->haveAllHashes())
                  this->fileWithoutHashes << file;

               currentFiles.removeOne(file);
            }
         }
      }

      // Deletes all the files and directories which doesn't exist on the file system.
      foreach (File* f, currentFiles)
         f->eliminate();
      foreach (Directory* d, currentSubDirs)
         d->eliminate();
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

   // TODO..
   LOG_DEBUG("File structure event occurs..");
}
