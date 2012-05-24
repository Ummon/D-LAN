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
  
#ifndef FILEMANAGER_ICHUNK_H
#define FILEMANAGER_ICHUNK_H

#include <QSharedPointer>

#include <Protos/common.pb.h>

#include <Common/Hash.h>
#include <Common/LogManager/ILoggable.h>

namespace FM
{
   class IDataReader;
   class IDataWriter;

   /**
     * A chunk of data that can be read or write.
     */
   class IChunk : public LM::ILoggable
   {
   public:
      virtual ~IChunk() {}

      /**
        * Ask the chunk to delete its own file. The file will be deleted only if unfinished.
        * Useful when user remove an unfinished download.
        */
      virtual void removeItsIncompleteFile() = 0;

      /**
        * Fill the entry object with its own file information.
        * @return false if the entry hasn't been populated.
        */
      virtual bool populateEntry(Protos::Common::Entry* entry) const = 0;

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
        * The exceptions (except 'UnableToOpenFileInWriteMode') may occur only if the settings
        * 'check_received_data_integrity' is true.
        * @exception FileResetException Occurs when the file has been created and we already got some known bytes.
        * @exception UnableToOpenFileInWriteMode
        * @exception IOErrorException
        * @exception ChunkDeletedException
        * @exception ChunkDataUnknownException
        */
      virtual QSharedPointer<IDataWriter> getDataWriter() = 0;

      /**
        * Number of the chunk, start at 0.
        * The chunk number 0 is the first data chunk in a file and the chunk number 'getNbTotalChunk() - 1' is the last one.
        */
      virtual int getNum() const = 0;

      /**
        * Returns the total number of chunk.
        */
      virtual int getNbTotalChunk() const = 0;

      /**
        * Returns the hash of the chunk, can be null.
        */
      virtual Common::Hash getHash() const = 0;

      /**
        * Set the hash of the chunk.
        */
      virtual void setHash(const Common::Hash&) = 0;

      virtual int getKnownBytes() const = 0;

      virtual int getChunkSize() const = 0;

      /**
        * Returns 'true' if 'getKnownBytes()' == 'getChunkSize()'.
        */
      virtual bool isComplete() const = 0;
   };
}
#endif
