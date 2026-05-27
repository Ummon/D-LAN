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
#include <QList>
#include <QString>

#include <Protos/common.pb.h>

#include <Common/Path.h>
#include <Common/Hashes.h>

namespace HC
{
   class IHashCache
   {
   public:
      virtual ~IHashCache() {}

      /**
        * Try to retrieve hashes from a file path.
        * Some hashes may be null if unknown.
        * If the file path is unknown a empty list is returned.
        */
      virtual Common::Hashes getHashes(const QString& filePath) = 0;

      virtual void setHashes(QString& filePath, const QList<QString>& filePaths) = 0;
      virtual void setSizeAndDateTime(QString& filePath, qint64 size, QDateTime dateTime) = 0;

      virtual void rmHashes(QString& filePath) = 0;

      // virtual QList<Common::Hashes> getHashes(const QList<QString>& filePaths);
      // virtual void setHashes(const QList<QString>& filePaths, const QList<Common::Hashes>& hashes);
      // virtual void rmHashes(const QList<QString>& filePaths);
   };
}
