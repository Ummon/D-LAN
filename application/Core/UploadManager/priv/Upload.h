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
  
#ifndef UPLOADMANAGER_UPLOAD_H
#define UPLOADMANAGER_UPLOAD_H

#include <QMutex>
#include <QThread>

#include <Common/Timeoutable.h>
#include <Common/TransferRateCalculator.h>
#include <Common/IRunnable.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/IDataReader.h>
#include <Core/PeerManager/ISocket.h>

#include <IUpload.h>

namespace UM
{
   class Upload : public Common::IRunnable, public IUpload, public Common::Timeoutable
   {
      static quint64 currentID; ///< Used to generate the new upload ID.

   public:
      Upload(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket, Common::TransferRateCalculator& transferRateCalculator);
      ~Upload();

      quint64 getID() const;
      Common::Hash getPeerID() const;
      int getProgress() const;
      QSharedPointer<FM::IChunk> getChunk() const;

      void init(QThread* thread);
      void run();
      void finished();
      void stop();

   private:
      mutable QMutex mutex;

      QThread* mainThread;

      const quint64 ID; ///< Each uploader has an ID to identified it.
      QSharedPointer<FM::IChunk> chunk; ///< The chunk uploaded.
      int offset; ///< The current offset into the chunk.
      QSharedPointer<PM::ISocket> socket;

      Common::TransferRateCalculator& transferRateCalculator;

      bool closeTheSocket;
      bool toStop;
   };
}

#endif
