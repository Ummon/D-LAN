#include <priv/FileUpdater.h>
using namespace FileManager;

#include <QLinkedList>
#include <QDir>

#include <priv/DirWatcher.h>
#include <priv/SharedDirectory.h>
#include <priv/Directory.h>
#include <priv/File.h>

FileUpdater::FileUpdater(FileManager* fileManager)
   : fileManager(fileManager), dirWatcher(DirWatcher::getNewWatcher())
{
}

void FileUpdater::addRoot(SharedDirectory* dir)
{
   this->mutex.lock();
   if (this->dirWatcher)
      this->dirWatcher->addDir(dir->getPath());

   this->dirsToScan.append(dir);

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

      }
   }
}

void FileUpdater::scan(SharedDirectory* dir)
{
   QLinkedList<Directory*> dirsToVisit;
   dirsToVisit.append(dir);

   while (!dirsToVisit.isEmpty())
   {
      Directory* currentDir = dirsToVisit.takeFirst();
      foreach (QFileInfo entry, QDir(currentDir->getPath()).entryInfoList())
      {
         if (entry.fileName() == "." || entry.fileName() == "..")
            continue;

         if (entry.isDir())
         {
            dirsToVisit.append(new Directory(currentDir, entry.baseName()));
         }
         else
         {
            new File(currentDir, entry.fileName(), entry.size());
         }
      }
   }
}
