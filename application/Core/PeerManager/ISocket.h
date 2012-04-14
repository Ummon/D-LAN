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
  
#ifndef PEERMANAGER_ISOCKET_H
#define PEERMANAGER_ISOCKET_H

#include <QtGlobal>
#include <QByteArray>

#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/Network/MessageSocket.h>

namespace PM
{
   class ISocket
   {
   public:
      virtual ~ISocket() {}

      virtual void setReadBufferSize(qint64 size) = 0;

      virtual qint64 bytesAvailable() const = 0;
      virtual qint64 read(char* data, qint64 maxSize) = 0;
      virtual QByteArray readAll() = 0;
      virtual bool waitForReadyRead(int msecs) = 0;

      virtual qint64 bytesToWrite() const = 0;
      virtual qint64 write(const char* data, qint64 maxSize) = 0;
      virtual qint64 write(const QByteArray& byteArray) = 0;
      virtual bool waitForBytesWritten(int msecs) = 0;

      virtual void moveToThread(QThread* targetThread) = 0;
      virtual QString errorString() const = 0;

      /**
        * Returns the ID of the remote peer on which the socket is connected.
        */
      virtual Common::Hash getRemotePeerID() const = 0;

      /**
        * Used by uploader to tell when an upload is finished.
        * TODO: should be removed and only be called by the peerManager (as with downloads).
        * @param closeTheSocket If true force the socket to be closed.
        */
      virtual void finished(bool closeTheSocket = false) = 0;
   };
}

#endif
