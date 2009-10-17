#include <priv/FileUpdater/FileUpdater.h>
using namespace FileManager;

#include <QLinkedList>
#include <QDir>
#include <QTime>

#include <priv/Cache/SharedDirectory.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>

FileUpdater::FileUpdater(FileManager* fileManager)
   : fileManager(fileManager), dirWatcher(DirWatcher::getNewWatcher())
{
}

void FileUpdater::addRoot(SharedDirectory* dir)
{
   this->mutex.lock();
   if (this->dirWatcher)
      this->dirWatcher->addDir(dir->getPath());

   this->dirsToScan << dir;

   this->dirNotEmpty.wakeOne();
   this->mutex.unlock();
}

void FileUpdater::rmRoot(SharedDirectory* dir)
{
   if (this->dirWatcher)
      this->dirWatcher->rmDir(dir->getPath());
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
         if (this->dirsToScan.empty())
            this->dirNotEmpty.wait(&this->mutex);

         SharedDirectory* addedDir = this->dirsToScan.takeLast();
         this->mutex.unlock();

         // Synchronize the new directory.
         this->scan(addedDir);
      }
      else // Wait for filesystem modifications.
      {
         this->mutex.unlock();

         if (this->dirsToScan.isEmpty())
            this->treatEvents(this->dirWatcher->waitEvent());
         else
            this->treatEvents(this->dirWatcher->waitEvent(0));
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

      if (time.elapsed() / 1000 >= minimumDurationWhenHashing)
         break;
   }
}

void FileUpdater::scan(SharedDirectory* dir)
{
   QLinkedList<Directory*> dirsToVisit;
   dirsToVisit << dir;

   while (!dirsToVisit.isEmpty())
   {
      Directory* currentDir = dirsToVisit.takeFirst();
      foreach (QFileInfo entry, QDir(currentDir->getPath()).entryInfoList())
      {
         if (entry.fileName() == "." || entry.fileName() == "..")
            continue;

         if (entry.isDir())
         {
            dirsToVisit << new Directory(currentDir, entry.baseName());
         }
         else
         {
            this->createNewFile(currentDir, entry.fileName(), entry.size());
         }
      }
   }
}

void FileUpdater::treatEvents(const QList<WatcherEvent>& events)
{

}
