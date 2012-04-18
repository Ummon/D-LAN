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
  
#ifndef FILEMANAGER_CHUNKS_H
#define FILEMANAGER_CHUNKS_H

#include <QHash>
#include <QSharedPointer>
#include <QMutex>

#include <Common/Hash.h>

namespace FM
{
   class Chunk;

   class Chunks : private QMultiHash<Common::Hash, QSharedPointer<Chunk>>
   {
   public:
      void add(QSharedPointer<Chunk> chunk);
      void rm(QSharedPointer<Chunk> chunk);
      QSharedPointer<Chunk> value(const Common::Hash& hash) const;
      QList<QSharedPointer<Chunk>> values(const Common::Hash& hash) const;
      bool contains(const Common::Hash& hash) const;

   private:
      mutable QMutex mutex; // From the documentation : "they (containers) are thread-safe in situations where they are used as read-only containers by all threads used to access them.".
   };
}
#endif
