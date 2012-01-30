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
  
#ifndef DOWNLOADMANAGER_DOWNLOAD_H
#define DOWNLOADMANAGER_DOWNLOAD_H

#include <QSharedPointer>
#include <QTimer>
#include <QFlags>

#include <Common/Uncopyable.h>
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
      Download(
         Common::Hash peerSourceID,
         const Protos::Common::Entry& remoteEntry,
         const Protos::Common::Entry& localEntry
      );

   public:
      virtual ~Download();

      /**
        * Set the download as active.
        */
      virtual void start() = 0;

      virtual void populateRemoteEntry(Protos::Queue::Queue_Entry* entry) const;
      virtual void populateLocalEntry(Protos::Queue::Queue_Entry* entry) const;

      void setPeer(PM::IPeer* peer);

      quint64 getID() const;
      Status getStatus() const;
      bool isStatusErroneous() const;
      void removeErroneousStatus();
      virtual int getProgress() const;
      Common::Hash getPeerSourceID() const;
      QSet<Common::Hash> getPeers() const;
      const Protos::Common::Entry& getRemoteEntry() const;
      const Protos::Common::Entry& getLocalEntry() const;

      void setAsDeleted();
      virtual void remove();

   protected:
      bool hasAValidPeer();

      /**
        * This method permits to change the behaviour by a subclass when Download change the status.
        */
      virtual void setStatus(Status newStatus);

   protected:
      const quint64 ID;

      const Common::Hash peerSourceID;
      PM::IPeer* peerSource;

      Protos::Common::Entry remoteEntry; ///< From.
      Protos::Common::Entry localEntry; ///< To.

      Status status;
   };
}
#endif
