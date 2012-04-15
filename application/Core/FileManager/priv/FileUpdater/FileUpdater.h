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
  
#ifndef FILEMANAGER_FILEUPDATER_H
#define FILEMANAGER_FILEUPDATER_H

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QString>
#include <QList>
#include <QElapsedTimer>

#include <Protos/files_cache.pb.h>

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/Cache/FileHasher.h>

namespace FM
{
   class FileManager;
   class SharedDirectory;
   class Directory;
   class File;
   class Entry;
   class WaitCondition;

   class FileUpdater : public QThread
   {
      Q_OBJECT

   public:
      FileUpdater(FileManager* fileManager);
      ~FileUpdater();

      void stop();
      void setFileCache(const Protos::FileCache::Hashes* fileCache);
      void prioritizeAFileToHash(File* file);

      bool isScanning() const;
      bool isHashing() const;
      int getProgress() const;

   public slots:
      void addRoot(SharedDirectory* dir);
      void rmRoot(SharedDirectory* dir, Directory* dir2 = 0);

   signals:
      void fileCacheLoaded();

   protected:
      void run();

   private:
      void computeSomeHashes();
      void updateHashingProgress();

      void stopHashing();

      void scan(Directory* dir, bool addUnfinished = false);

      void stopScanning(Directory* dir = 0);

      void deleteEntry(Entry* entry);
      void removeFromDirsToScan(Directory* dir);
      void removeFromFilesWithoutHashes(Directory* dir);

      void restoreFromFileCache(SharedDirectory* dir);

      bool treatEvents(const QList<WatcherEvent>& events);

      const int SCAN_PERIOD_UNWATCHABLE_DIRS;

      FileManager* fileManager;
      DirWatcher* dirWatcher;

      const Protos::FileCache::Hashes* fileCache; ///< The hashes from the saved file cache. Used only temporally at the begining of 'run()'.

      bool toStop; ///< Set to true when the service must be stopped.

      int progress;

      WaitCondition* dirEvent; ///< Using to wait when a sharing directory is added or deleted.
      mutable QMutex mutex; ///< Prevent the access from many thread to the internal data like 'filesWithoutHashes' for example.

      QList<Directory*> unwatchableDirs;
      QElapsedTimer timerScanUnwatchable;
      QList<Directory*> dirsToScan; ///< When a new shared directory is added, it is put in this list until it is scanned.
      Directory* currentScanningDir;
      QWaitCondition scanningStopped;
      mutable QMutex scanningMutex;

      mutable QMutex hashingMutex;
      bool toStopHashing;
      FileHasher fileHasher;

      QList<SharedDirectory*> dirsToRemove;

      QList<File*> filesWithoutHashes;
      QList<File*> filesWithoutHashesPrioritized;
      qint64 remainingSizeToHash;
   };
}
#endif
