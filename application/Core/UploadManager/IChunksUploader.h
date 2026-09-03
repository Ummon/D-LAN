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

#include <Core/FileManager/IChunk.h>
#include <Core/PeerManager/GetChunkParams.h>

#include <Common/Hash.h>

namespace UM
{
   class IChunksUploader
   {
   public:
      virtual ~IChunksUploader() {}

      /**
        * Returns the upload ID, it can be use later to retrieve an upload.
        */
      virtual quint64 getID() const = 0;

      /**
        * Returns the ID of the remote peer.
        */
      virtual Common::Hash getPeerID() const = 0;

      /**
        * Returns the chunks being uploaded and their current offset.
        */
      virtual QList<PM::GetChunkParams> getChunks() const = 0;
   };
}
