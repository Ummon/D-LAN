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

#include <QSharedPointer>
#include <QDateTime>
#include <QList>
#include <QString>

#include <Protos/common.pb.h>

#include <Common/Hash.h>
#include <Common/Path.h>

namespace HC
{
   class IHashCache
   {
   public:
      virtual ~IHashCache() {}

      /**
        * Try to retrieve all hashes from a file path.
        * If the file path is unknown a empty list is returned.
        */
      virtual QList<Common::Hash> getHashes(
         const QString& filePath,
         QDateTime timeLastModified = QDateTime()
      ) = 0;

      /**
        * Set all hashes for the given file path.
        * If the number of hased doesn't match the file size, the request is rejected.
        */
      virtual void setHashes(
         const QString& filePath,
         const QList<Common::Hash>& hashes,
         qint64 size,
         QDateTime dateTime = QDateTime()
      ) = 0;

      virtual void rmHashes(const QString& filePath) = 0;
   };
}
