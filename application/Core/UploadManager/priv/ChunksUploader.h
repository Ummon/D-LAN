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

#include <QMutex>
#include <QThread>

#include <Common/Timeoutable.h>
#include <Common/TransferRateCalculator.h>
#include <Common/IRunnable.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/IDataReader.h>
#include <Core/PeerManager/ISocket.h>

#include <IChunksUploader.h>

namespace UM
{
   class ChunksUploader : public Common::IRunnable, public IChunksUploader, public Common::Timeoutable
   {
      static quint64 currentID; ///< Used to generate the new upload ID.

   public:
      ChunksUploader(
         QList<std::pair<QSharedPointer<FM::IChunk>, int>> chunksAndOffsets,
         const QSharedPointer<PM::ISocket>& socket,
         Common::TransferRateCalculator& transferRateCalculator
      );

      ~ChunksUploader();

      quint64 getID() const override;
      Common::Hash getPeerID() const override;
      int getProgress() const override;
      QList<QSharedPointer<FM::IChunk>> getChunks() const override;

      void init(QThread* thread) override;
      void run() override;
      void finished() override;

      void stop();

   private:
      mutable QMutex mutex;

      QThread* mainThread;

      const quint64 ID; ///< Each uploader has an ID to identified it.
      QList<std::pair<QSharedPointer<FM::IChunk>, int>> chunks; ///< The chunks uploaded.
      // int offset; ///< The current offset into the chunk.
      QSharedPointer<PM::ISocket> socket;

      Common::TransferRateCalculator& transferRateCalculator;

      bool closeTheSocket;
      bool toStop;
   };
}
