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
  
#ifndef FILEMANAGER_ICHUNK_H
#define FILEMANAGER_ICHUNK_H

#include <QSharedPointer>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

namespace FM
{
   class IDataReader;
   class IDataWriter;

   class IChunk
   {
   public:
      virtual ~IChunk() {}

      /**
        * Remove only the file if it's incomplete.
        */
      virtual void removeItsFile() = 0;

      virtual void populateEntry(Protos::Common::Entry* entry) const = 0;

      /**
        * Returns the base path of the file. Not ending by a '/'.
        * The full path to the file corresponds to :
        * getAbsoluteBasePath() + entry.path + entry.name
        * See 'populateEntry(..)'.
        */
      virtual QString getBasePath() const = 0;

      /**
        * The caller must not delete the IChunk as long as data is read with the IDataReader.
        * @exception UnableToOpenFileInReadMode
        */
      virtual QSharedPointer<IDataReader> getDataReader() = 0;

      /**
        * The caller must not delete the IChunk as long as data is written with the IDataWriter.
        * @exception UnableToOpenFileInWriteMode
        */
      virtual QSharedPointer<IDataWriter> getDataWriter() = 0;

      /**
        * Send all the chunk to a socket.
        */
      //virtual void sendContentToSocket(QAbstractSocket& socket) = 0;

      /**
        * Read all the chunk from a socket.
        * If a chunk has already be filled but is not complete it will read
        */
      //virtual void getContentFromSocket(QAbstractSocket& socket) = 0;

      virtual int getNum() const = 0;

      virtual int getNbTotalChunk() const = 0;

      virtual Common::Hash getHash() const = 0;

      virtual void setHash(const Common::Hash&) = 0;

      virtual int getKnownBytes() const = 0;

      virtual int getChunkSize() const = 0;

      virtual bool isComplete() const = 0;

      virtual QString toStr() const = 0;
   };
}
#endif
