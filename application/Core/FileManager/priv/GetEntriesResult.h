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
  
#ifndef FILEMANAGER_GET_ENTRIES_RESULT_H
#define FILEMANAGER_GET_ENTRIES_RESULT_H

#include <QObject>

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
      GetEntriesResult(Directory* dir);
      void start();

   private slots:
      void directoryScanned(Directory* dir);
      void sendResult();

   private:
      void buildResult();

      Protos::Core::GetEntriesResult::EntryResult res;
      Directory* dir;
   };
}

#endif
