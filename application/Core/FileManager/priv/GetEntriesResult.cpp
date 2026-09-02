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

#include <Common/Settings.h>

#include <priv/Log.h>

/**
  * @class FM::GetEntriesResult
  *
  * Gives the content of a directory, waiting for it to be scanned if needed.
  * 'directoryScanned(..)' and 'entryRemoved(..)' are called from the thread which modifies the cache (usually
  * the 'FileUpdater' one) while 'start()' and 'sendResult()' are called from the main thread, hence the mutex.
  */

GetEntriesResult::GetEntriesResult(Directory* dir, int maxNbHashesPerEntry) :
   IGetEntriesResult(SETTINGS.get<quint32>("get_entries_timeout")),
   dir(dir),
   cache(dir ? dir->getCache() : nullptr),
   maxNbHashesPerEntry(maxNbHashesPerEntry),
   resultBuilt(false)
{
}

GetEntriesResult::~GetEntriesResult()
{
   // Waits for a slot running in another thread to finish before the object is destroyed.
   QMutexLocker locker(&this->mutex);
   this->disconnectFromCache();
}

void GetEntriesResult::start()
{
   if (!this->dir)
   {
      L_DEBU("FM::GetEntriesResult::start(): null directory");
      this->res.set_status(Protos::Core::GetEntriesResult::EntryResult::DONT_HAVE);
      emit result(this->res);
      return;
   }

   // Connected before testing 'isScanned()' to not miss a scan ending in between.
   connect(this->cache, &Cache::entryRemoved, this, &GetEntriesResult::entryRemoved, Qt::DirectConnection);
   connect(this->cache, &Cache::directoryScanned, this, &GetEntriesResult::directoryScanned, Qt::DirectConnection);

   if (this->dir->isScanned())
   {
      // The directory is not accessed while holding 'mutex': the cache thread locks the directory before calling our slots.
      {
         QMutexLocker locker(&this->mutex);
         if (this->resultBuilt) // A slot has been faster and will send the result.
            return;
         this->resultBuilt = true;
      }

      L_DEBU(QString("FM::GetEntriesResult::start(): directory scanned: %1").arg(this->dir->getAbsolutePath()));
      this->buildResult();
      this->disconnectFromCache();
      emit result(this->res);
   }
   else
   {
      L_DEBU(QString("FM::GetEntriesResult::start(): directory not yet scanned: %1").arg(this->dir->getAbsolutePath()));
      this->startTimer();
   }
}

/**
  * Called from the thread which has scanned the directory, the directory is locked by the caller.
  */
void GetEntriesResult::directoryScanned(FM::Directory* dir)
{
   QMutexLocker locker(&this->mutex);

   if (dir != this->dir || this->resultBuilt)
      return;

   L_DEBU(QString("FM::GetEntriesResult::directoryScanned(): directory just scanned: %1").arg(this->dir->getAbsolutePath()));

   this->resultBuilt = true;
   this->buildResult();
   this->disconnectFromCache();

   QMetaObject::invokeMethod(this, "sendResult", Qt::QueuedConnection); // To send the message 'result' in the main thread.
}

/**
  * Called from the thread which removes the entry, the entry is locked by the caller.
  * The directory is going to be deleted, it must not be accessed anymore.
  */
void GetEntriesResult::entryRemoved(FM::Entry* entry)
{
   QMutexLocker locker(&this->mutex);

   if (entry != this->dir || this->resultBuilt)
      return;

   L_DEBU(QString("FM::GetEntriesResult::entryRemoved(): directory removed before being scanned: %1").arg(this->dir->getAbsolutePath()));

   this->dir = nullptr;
   this->resultBuilt = true;
   this->res.set_status(Protos::Core::GetEntriesResult::EntryResult::DONT_HAVE);
   this->disconnectFromCache();

   QMetaObject::invokeMethod(this, "sendResult", Qt::QueuedConnection);
}

void GetEntriesResult::sendResult()
{
   this->stopTimer();

   L_DEBU("FM::GetEntriesResult::sendResult()");
   emit result(this->res);
}

void GetEntriesResult::buildResult()
{
   this->res.set_status(Protos::Core::GetEntriesResult::EntryResult::OK);
   this->res.mutable_entries()->clear_entries();

   foreach (Directory* dir, this->dir->getSubDirs())
      dir->populateEntry(this->res.mutable_entries()->add_entries());

   foreach (File* file, this->dir->getFiles())
      if (file->isComplete())
         file->populateEntry(this->res.mutable_entries()->add_entries(), false, this->maxNbHashesPerEntry);
}

void GetEntriesResult::disconnectFromCache()
{
   if (!this->cache)
      return;

   disconnect(this->cache, &Cache::entryRemoved, this, &GetEntriesResult::entryRemoved);
   disconnect(this->cache, &Cache::directoryScanned, this, &GetEntriesResult::directoryScanned);
}
