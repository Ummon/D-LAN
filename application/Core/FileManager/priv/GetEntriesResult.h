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

#include <QObject>
#include <QMutex>

#include <Common/Uncopyable.h>

#include <IGetEntriesResult.h>
#include <priv/Cache/Cache.h>
#include <priv/Cache/Directory.h>

namespace FM
{
   class GetEntriesResult : public IGetEntriesResult, Common::Uncopyable
   {
      Q_OBJECT
   public:
      GetEntriesResult(Directory* dir, int maxNbHashesPerEntry);
      ~GetEntriesResult();
      void start();

   private slots:
      void directoryScanned(FM::Directory* dir);
      void entryRemoved(FM::Entry* entry);
      void sendResult();

   private:
      void buildResult();
      void disconnectFromCache();

      Protos::Core::GetEntriesResult::EntryResult res;
      Directory* dir; ///< Set to 'nullptr' when the directory is removed from the cache.
      Cache* cache;
      const int maxNbHashesPerEntry;

      QMutex mutex; ///< Protects 'dir', 'res' and 'resultBuilt' from concurrent slot calls.
      bool resultBuilt; ///< Once set the result is final and cannot be built or invalidated anymore.
   };
}
