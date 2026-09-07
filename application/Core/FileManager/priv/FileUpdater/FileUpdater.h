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

#pragma once

#include <atomic>

#include <QThread>
#include <QWaitCondition>
#include <QMutex>
#include <QRecursiveMutex>
#include <QString>
#include <QList>
#include <QElapsedTimer>
#include <QFileInfo>

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/FileUpdater/HashingQueue.h>
#include <priv/Cache/FileHasher.h>

class CacheTest;

namespace FM
{
   class FileManager;
   class SharedEntry;
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
      void prioritizeAFileToHash(File* file);

      bool isScanning() const;
      bool isHashing() const;
      int getProgress() const;

      void addRoot(SharedEntry* sharedEntry);
      void rmRoot(SharedEntry* sharedEntry, Directory* dir = nullptr);
      void prepareToDeleteEntry(Entry* entry);

   signals:
      void initialScanFinished();
      void deleteSharedEntry(FM::SharedEntry* sharedEntry);

   protected:
      void run();

   private:
      friend class ::CacheTest; // Exercise scheduler transitions without running its event loop.
      void computeSomeHashes();
      int nextWaitTimeout(qint64 elapsedSinceScan, qint64 now) const;

      void stopHashing();

      void scan(Entry* entry, bool addUnfinished = false);

      File* addScannedFile(const QFileInfo& fileInfo, File* file, Directory* parentDirectory = nullptr);

      void stopScanning(Entry* entry = nullptr);

      void deleteEntry(Entry* entry);
      void removeFromEntriesToScan(Entry* entry);
      void removeFromHashingQueue(Entry* entry);

      bool processEvents(const QList<WatcherEvent>& events);

      const int SCAN_PERIOD_UNWATCHABLE_DIRS;
      const int IO_ERROR_WAITING_BEFORE_RETRY;

      FileManager* fileManager;
      DirWatcher* dirWatcher;

      std::atomic<bool> toStop; ///< Set to true when the service must be stopped.

      WaitCondition* dirEvent; ///< Using to wait when a sharing directory is added or deleted.
      mutable QRecursiveMutex mutex; ///< Protects scanning queues and all hashing scheduler access.

      QList<Entry*> unwatchableEntries;
      QElapsedTimer timerScanUnwatchable;

      QList<Entry*> entriesToScan; ///< When something change in a directory or in a file we put it in this list until it is scanned.
      Entry* currentScanningEntry;

      QWaitCondition scanningStopped;      
      mutable QMutex scanningMutex;
      std::atomic<bool> scanAbortRequested { false };

      mutable QMutex hashingMutex; ///< Protects selected-file lifetime through hashing and scheduler bookkeeping.
      std::atomic<bool> toStopHashing;
      FileHasher fileHasher;

      QList<Entry*> rootEntriesToRemove;

      HashingQueue hashingQueue;
      QElapsedTimer schedulerClock;
   };
}
