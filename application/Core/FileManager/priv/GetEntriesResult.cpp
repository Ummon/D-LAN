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

GetEntriesResult::GetEntriesResult(Directory* dir) :
   IGetEntriesResult(SETTINGS.get<quint32>("get_entries_timeout")), dir(dir)
{
}

void GetEntriesResult::start()
{
   if (!this->dir)
   {
      L_DEBU("FM::GetEntriesResult::start(): null directory");
      emit result(this->res);
   }
   else if (this->dir->isScanned())
   {
      L_DEBU(QString("FM::GetEntriesResult::start(): directory scanned: %1").arg(this->dir->getFullPath()));
      this->buildResult();
      emit result(this->res);
   }
   else
   {
      L_DEBU(QString("FM::GetEntriesResult::start(): directory not yet scanned: %1").arg(this->dir->getFullPath()));
      connect(this->dir->getCache(), SIGNAL(directoryScanned(Directory*)), this, SLOT(directoryScanned(Directory*)), Qt::DirectConnection);
      this->startTimer();
   }
}

/**
  * This method is called in the 'FileUpdater' thread.
  */
void GetEntriesResult::directoryScanned(Directory* dir)
{
   if (dir != this->dir)
      return;

   L_DEBU(QString("FM::GetEntriesResult::directoryScanned(): directory just scanned: %1").arg(this->dir->getFullPath()));

   this->buildResult();

   QMetaObject::invokeMethod(this, "sendResult"); // To send the message 'result' in the main thread.
}

void GetEntriesResult::sendResult()
{
   disconnect(this->dir->getCache(), SIGNAL(directoryScanned(Directory*)), this, SLOT(directoryScanned(Directory*)));
   this->stopTimer();

   L_DEBU("FM::GetEntriesResult::sendResult()");
   emit result(res);
}

void GetEntriesResult::buildResult()
{
   foreach (Directory* dir, this->dir->getSubDirs())
      dir->populateEntry(this->res.add_entry());

   foreach (File* file, this->dir->getFiles())
      if (file->isComplete())
         file->populateEntry(this->res.add_entry());
}
