/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef UPLOADMANAGER_IUPLOAD_H
#define UPLOADMANAGER_IUPLOAD_H

#include <QSharedPointer>

#include <Core/FileManager/IChunk.h>

#include <Common/Hash.h>

namespace UM
{
   class IPeer;
   class IChunk;

   class IUpload
   {
   public:
      virtual ~IUpload() {}

      /**
        * Returns the upload ID, it can be use later to retrieve an upload.
        */
      virtual quint64 getID() const = 0;

      /**
        * Returns the ID of the remote peer.
        */
      virtual Common::Hash getPeerID() const = 0;

      /**
        * Returns a value between 0 and 100.
        */
      virtual int getProgress() const = 0;

      /**
        * Returns the chunk being uploaded.
        */
      virtual QSharedPointer<FM::IChunk> getChunk() const = 0;
   };
}
#endif
