#include <priv/FileUpdater/FileUpdater.h>
using namespace FM;

#include <QLinkedList>
#include <QDir>
#include <QTime>

#include <priv/FileManager.h>
#include <priv/Exceptions.h>
#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>
#include <priv/FileUpdater/WaitCondition.h>

FileUpdater::FileUpdater(FileManager* fileManager)
   : fileManager(fileManager), dirWatcher(DirWatcher::getNewWatcher())
{
   this->dirNotEmpty = WaitCondition::getNewWaitCondition();
}

FileUpdater::~FileUpdater()
{
   if (this->dirNotEmpty)
      delete this->dirNotEmpty;
}

void FileUpdater::addRoot(SharedDirectory* dir)
{
   QMutexLocker(&this->mutex);

   if (this->dirWatcher)
      this->dirWatcher->addDir(dir->getFullPath());
   else
      if (!QDir(dir->getFullPath()).exists())
         throw DirNotFoundException(dir->getFullPath());

   this->dirsToScan << dir;

   this->dirNotEmpty->release();
}

void FileUpdater::rmRoot(SharedDirectory* dir)
{
   QMutexLocker(&this->mutex);

   if (this->dirWatcher)
      this->dirWatcher->rmDir(dir->getFullPath());
}

void FileUpdater::run()
{
   forever
   {
      this->computeSomeHashes();

      this->mutex.lock();

      // If there is no watcher capability or no directory to watch then
      // we wait for an added directory.
      if (!this->dirWatcher || this->dirWatcher->nbWatchedDir() == 0 || !this->dirsToScan.empty())
      {
         if (this->dirsToScan.isEmpty())
         {
            LOG_DEBUG("Waiting for a new shared directory added..");
            this->mutex.unlock();

            this->dirNotEmpty->wait();
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
            this->treatEvents(this->dirWatcher->waitEvent(this->dirNotEmpty));
         }
         else
         {
            this->mutex.unlock();
            this->treatEvents(this->dirWatcher->waitEvent(0));
         }
      }
   }
}

void FileUpdater::createNewFile(Directory* dir, const QString& filename, qint64 size)
{
   File* file = new File(dir, filename, size);
   this->fileWithoutHashes << file;
}

void FileUpdater::computeSomeHashes()
{
   QTime time;
   time.start();

   QMutableLinkedListIterator<File*> file(this->fileWithoutHashes);
   while (file.hasNext())
   {
      file.next()->computeHashes();
      file.remove();

      if (time.elapsed() / 1000 >= MINIMUM_DURATION_WHEN_HASHING)
         break;
   }
}

void FileUpdater::scan(SharedDirectory* dir)
{
   LOG_DEBUG("Start scanning a shared directory : " + dir->getFullPath());
   QLinkedList<Directory*> dirsToVisit;
   dirsToVisit << dir;

   while (!dirsToVisit.isEmpty())
   {
      Directory* currentDir = dirsToVisit.takeFirst();
      foreach (QFileInfo entry, QDir(currentDir->getFullPath()).entryInfoList())
      {
         if (entry.fileName() == "." || entry.fileName() == "..")
            continue;

         if (entry.isDir())
         {
            dirsToVisit << new Directory(currentDir, entry.fileName());
         }
         else
         {
            this->createNewFile(currentDir, entry.fileName(), entry.size());
         }
      }
   }
   LOG_DEBUG("Scan terminated : " + dir->getFullPath());
}

void FileUpdater::treatEvents(const QList<WatcherEvent>& events)
{
   // TODO something
   LOG_DEBUG("File structure event occurs");
}
