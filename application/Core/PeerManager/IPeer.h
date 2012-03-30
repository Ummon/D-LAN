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
  
#ifndef PEERMANAGER_IPEER_H
#define PEERMANAGER_IPEER_H

#include <QObject>
#include <QSharedPointer>
#include <QHostAddress>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <Common/Hashes.h>
#include <Common/LogManager/ILoggable.h>
#include <Core/PeerManager/IGetEntriesResult.h>
#include <Core/PeerManager/IGetHashesResult.h>
#include <Core/PeerManager/IGetChunkResult.h>

namespace PM
{
   class IGetHashes;

   /**
     * @brief A remote peer.
     *
     * This class owns some information about a remote peer like its ID, IP, nick, etc.
     *
     * It's possible to ask to a remote peer three things:
     *  - The sub entries of a given entry (browse).
     *  - The hashes of a given entry. This entry must be a file.
     *  - The data of a given chunk hash.
     *
     * A peer is never deleted, it's safe to keep a pointer.
     */
   class IPeer : public LM::ILoggable
   {
   public:
      virtual ~IPeer() {}

      virtual Common::Hash getID() const = 0;
      virtual QHostAddress getIP() const = 0;
      virtual quint16 getPort() const = 0;
      virtual QString getNick() const = 0;

      /**
        * May return a null QString if version is unknown.
        */
      virtual QString getCoreVersion() const = 0;

      virtual quint64 getSharingAmount() const = 0;

      /**
        * Return the average speed when downloading from this peer.
        * [bytes/s].
        * The default speed is 2^32-1.
        */
      virtual quint32 getSpeed() = 0;

      /**
        * When we download a file from a peer we can set its current speed with this method.
        * If this method isn't called for some time, the speed will be reset to its default value.
        * See 'DownloadRateValidTime' from this wiki page: http://dev.d-lan.net/projects/pmp/wiki/Protocol_core-core#Parameters to show the computation.
        */
      virtual void setSpeed(quint32 newSpeed) = 0;

      /**
        * Ban a peer for a given duration [ms].
        * 'isAvailable()' will return false while the duration.
        */
      virtual void ban(int duration, const QString& reason = QString()) = 0;

      /**
        * If we don't receive an IMAlive message from a peer during a certain time (for example 20 seconds), it will be concidered as dead.
        */
      virtual bool isAlive() const = 0;

      /**
        * True if the peer is alive and not banned.
        */
      virtual bool isAvailable() const = 0;

      /**
        * Ask for the entries in a given directories.
        * This method is non-blocking, the entries will be delivered by the signal
        * 'entriesResult'.
        * If a second getEntries
        */
      virtual QSharedPointer<IGetEntriesResult> getEntries(const Protos::Core::GetEntries& dirs) = 0;

      /**
        * Ask for the hashes of a given file.
        * This method is non-blocking, the hashes will be delivered by the signal 'nextHashResult' followed by
        * one or more 'nextHash' signal.
        */
      virtual QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file) = 0;

      /**
        * Ask to download a chunk.
        * @param deleteLater If you want to use 'QObject::deleteLate()' instead of the simple 'delete'.
        */
      virtual QSharedPointer<IGetChunkResult> getChunk(const Protos::Core::GetChunk& chunk) = 0;
   };
}
#endif
