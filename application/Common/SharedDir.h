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
  
#ifndef COMMON_SHAREDDIR_H
#define COMMON_SHAREDDIR_H

#include <QString>
#include <QMetaType>

#include <Common/Hash.h>

namespace Common
{
   /**
     * A little helper type for a local shared directory.
     * A tuple (ID, path) that identify a SharedDirectory.
     * Used by FM::IFileManager.
     */
   struct SharedDir
   {
      bool isNull() const { return ID.isNull(); }

      bool operator==(const SharedDir& other) const { return this->ID == other.ID; }
      bool equalTo(const SharedDir& other) const { return this->ID == other.ID && this->path == other.path && this->size == other.size && this->freeSpace == other.freeSpace; }

      Common::Hash ID; ///< The unique identifier of the shared directory.
      QString path; ///< The absolute path of the shared directory.
      qint64 size;
      qint64 freeSpace;
   };
}

Q_DECLARE_METATYPE(Common::SharedDir)

#endif
