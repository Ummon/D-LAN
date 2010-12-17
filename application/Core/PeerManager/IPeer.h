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

   class IPeer : public LM::ILoggable
   {
   public:
      virtual ~IPeer() {}

      virtual Common::Hash getID() const = 0;
      virtual QHostAddress getIP() const = 0;
      virtual quint16 getPort() const = 0;
      virtual QString getNick() const = 0;
      virtual quint64 getSharingAmount() const = 0;

      /**
        * Return the average speed when downloading from this peer.
        * [bytes/s].
        * The default speed is 2^32-1.
        */
      virtual quint32 getSpeed() = 0;

      virtual void setSpeed(quint32 speed) = 0;

      virtual bool isAlive() = 0;

      /**
        * Ask for the entries in a given directory.
        * This method is non-blocking, the entries will be delivered by the signal
        * 'entriesResult'.
        * If a second getEntries
        */
      virtual QSharedPointer<IGetEntriesResult> getEntries(const Protos::Core::GetEntries& dir) = 0;

      /**
        * Ask for the hashes of a given file.
        * This method is non-blocking, the hashes will be delivered by the signal 'nextHashResult' followed by
        * one or more 'nextHash' signal.
        */
      virtual QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file) = 0;

      /**
        * Ask to download a chunk.
        */
      virtual QSharedPointer<IGetChunkResult> getChunk(const Protos::Core::GetChunk& chunk) = 0;
   };
}
#endif
