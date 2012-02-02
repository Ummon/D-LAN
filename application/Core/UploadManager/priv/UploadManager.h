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
  
#ifndef UPLOADMANAGER_UPLOADMANAGER_H
#define UPLOADMANAGER_UPLOADMANAGER_H

#include <QSharedPointer>
#include <QList>

#include <Common/Uncopyable.h>
#include <Common/Hash.h>
#include <Common/ThreadPool.h>
#include <Common/TransferRateCalculator.h>
#include <Core/PeerManager/IPeerManager.h>

#include <IUploadManager.h>
#include <priv/Log.h>

namespace UM
{
   class Upload;

   class UploadManager : public QObject, public IUploadManager, Common::Uncopyable
   {
      Q_OBJECT
   public:
      UploadManager(QSharedPointer<PM::IPeerManager> peerManager);
      ~UploadManager();

      QList<IUpload*> getUploads() const;

      int getUploadRate();

   private slots:
      void getChunk(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket);
      void uploadTimeout();

   private:
      LOG_INIT_H("UploadManager");

      QSharedPointer<PM::IPeerManager> peerManager;

      QList< QSharedPointer<Upload> > uploads;

      Common::ThreadPool threadPool;

      Common::TransferRateCalculator transferRateCalculator;
   };
}
#endif
