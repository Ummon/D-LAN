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
  
#ifndef FILEMANAGER_FILEHASHER_H
#define FILEMANAGER_FILEHASHER_H

#include <QObject>
#include <QMutex>
#include <QWaitCondition>

#include <Common/Uncopyable.h>

#include <priv/Cache/FilePool.h>

namespace FM
{
   class Entry;
   class FileForHasher;

   class FileHasher : public QObject, Common::Uncopyable
   {
      Q_OBJECT
   public:
      FileHasher();

      bool start(FileForHasher* fileCache, int n = 0, int* amountHashed = nullptr);
      void stop();

   private slots:
      void entryRemoved(Entry* entry);

   private:
      void internalStop();

      FileForHasher* currentFileCache;

      bool hashing;
      bool toStopHashing;
      QWaitCondition hashingStopped;
      QMutex hashingMutex;

      static FilePool filePool;
   };
}

#endif
