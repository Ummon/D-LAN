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
  
#ifndef DOWNLOADMANAGER_DOWNLOAD_H
#define DOWNLOADMANAGER_DOWNLOAD_H

#include <QSharedPointer>
#include <QTimer>
#include <QFlags>

#include <Common/Uncopyable.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IPeer.h>

#include <Protos/common.pb.h>
#include <Protos/queue.pb.h>

#include <IDownload.h>

namespace PM { class IPeer; }

namespace DM
{
   class Download : public QObject, public IDownload, Common::Uncopyable
   {
      Q_OBJECT
      static quint64 currentID;

   protected:
      Download(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry);

   public:
      virtual ~Download();

      virtual void populateEntry(Protos::Queue::Queue_Entry* entry) const;

      quint64 getID() const;
      Status getStatus() const;
      bool isStatusErroneous() const;
      virtual int getProgress() const;
      Common::Hash getPeerSourceID() const;
      const Protos::Common::Entry& getEntry();
      void remove();

      bool hasAValidPeer();

   signals:
      void deleted(Download*);

   protected slots:
      virtual void retrievePeer();

   protected:
      /**
        * This method permits to change the behaviour by a subclass when Download change the status.
        */
      virtual void setStatus(Status newStatus);

      const quint64 ID;

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      Common::Hash peerSourceID;
      PM::IPeer* peerSource;
      Protos::Common::Entry entry;

      Status status;
   };
}
#endif
