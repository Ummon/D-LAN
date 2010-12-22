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
  
#ifndef UPLOADMANAGER_UPLOADER_H
#define UPLOADMANAGER_UPLOADER_H

#include <QThread>
#include <QSharedPointer>
#include <QTimer>
#include <QMutex>

#include <Common/Uncopyable.h>
#include <Common/TransferRateCalculator.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/IDataReader.h>
#include <Core/PeerManager/ISocket.h>

#include <IUpload.h>

namespace UM
{
   class Uploader : public QThread, public IUpload, Common::Uncopyable
   {
      Q_OBJECT
      static quint64 currentID;

   public:
      Uploader(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket);

      quint64 getID() const;
      int getUploadRate() const;
      Common::Hash getPeerID() const;
      int getProgress() const;
      QSharedPointer<FM::IChunk> getChunk() const;
      QSharedPointer<PM::ISocket> getSocket() const;
      void startTimer();

   signals:
      void uploadFinished(bool error);
      void uploadTimeout();

   protected:
      void run();

   private:
      const quint64 ID;
      QSharedPointer<FM::IChunk> chunk;
      int offset;
      QSharedPointer<PM::ISocket> socket;
      QTimer timer;
      mutable QMutex mutex;

      Common::TransferRateCalculator transferRateCalculator;

      QThread* mainThread;
   };
}
#endif
