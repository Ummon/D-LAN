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

#include  <Core/FileManager/IChunk.h>

namespace PM
{
   /**
     * The parameters of a chunk asked by a remote peer, see 'IPeerManager::getChunks(..)'.
     * The data to send is the interval [offset, endOffset) relative to the chunk, 'offset' being moved
     * forward by the uploader as the data is sent.
     */
   class GetChunkParams
   {
   public:
      GetChunkParams(QSharedPointer<FM::IChunk> chunk, int offset, int endOffset, qint64 fileBytesOwnedByPeer);

      QSharedPointer<FM::IChunk> getChunk() const;

      int getOffset() const;
      void setOffset(int offset);

      /**
        * The offset, relative to the chunk, at which the upload must stop. It corresponds to the size
        * announced to the peer ('Protos::Core::GetChunksResult::ChunkResult::chunk_size') and must not be
        * exceeded: the peer reads exactly this amount of data.
        */
      int getEndOffset() const;

      qint64 getFileBytesOwnedByPeer() const;

   private:
      QSharedPointer<FM::IChunk> chunk;
      int offset;
      int endOffset;
      qint64 fileBytesOwnedByPeer;
   };
}