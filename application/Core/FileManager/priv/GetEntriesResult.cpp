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

#include <priv/GetEntriesResult.h>
using namespace FM;

#include <QThread>
#include <Common/Settings.h>

/**
  * Browse requests and physical cache-entry deletion run on the cache's thread.
  * Worker notifications only schedule a fresh lookup; they never pass a directory pointer
  * to code that dereferences it later. No event processing occurs while building the snapshot,
  * so queued deletion cannot destroy the directory or its children during that operation.
  */
GetEntriesResult::GetEntriesResult(Cache& cache, const Protos::Common::Entry& directory, int maxNbHashesPerEntry) :
   IGetEntriesResult(SETTINGS.get<quint32>("get_entries_timeout")),
   directory(directory),
   cache(&cache),
   maxNbHashesPerEntry(maxNbHashesPerEntry)
{
   Q_ASSERT(this->thread() == cache.thread());
}

void GetEntriesResult::start()
{
   Q_ASSERT(QThread::currentThread() == this->thread());
   if (this->started)
      return;
   this->started = true;

   if (this->cache)
   {
      // Subscribe before checking scan state so a scan finishing during start is not missed.
      connect(this->cache, &Cache::directoryScanned, this, &GetEntriesResult::tryBuildResult, Qt::QueuedConnection);
      connect(this->cache, &Cache::entryRemoved, this, &GetEntriesResult::tryBuildResult, Qt::QueuedConnection);
      connect(this->cache, &QObject::destroyed, this, &GetEntriesResult::tryBuildResult, Qt::QueuedConnection);
   }

   this->startTimer();
   this->tryBuildResult();
}

void GetEntriesResult::tryBuildResult()
{
   Q_ASSERT(QThread::currentThread() == this->thread());
   Q_ASSERT(!this->cache || this->cache->thread() == this->thread());
   if (!this->started || this->finished || this->isTimedout())
      return;

   Directory* dir = this->cache ? this->cache->getDirectory(this->directory) : nullptr;
   if (dir && !dir->isScanned())
      return;

   Protos::Core::GetEntriesResult::EntryResult res;
   if (!dir)
      res.set_status(Protos::Core::GetEntriesResult::EntryResult::DONT_HAVE);
   else
   {
      res.set_status(Protos::Core::GetEntriesResult::EntryResult::OK);
      for (Directory* child : dir->getSubDirs())
         child->populateEntry(res.mutable_entries()->add_entries());
      for (File* file : dir->getFiles())
         if (file->isComplete())
            file->populateEntry(res.mutable_entries()->add_entries(), false, this->maxNbHashesPerEntry);
   }

   this->finished = true;
   this->stopTimer();
   this->disconnectFromCache();
   // Keep the response on the stack: a receiver may destroy this request synchronously.
   emit result(res);
}

void GetEntriesResult::disconnectFromCache()
{
   if (this->cache)
      disconnect(this->cache, nullptr, this, nullptr);
}
