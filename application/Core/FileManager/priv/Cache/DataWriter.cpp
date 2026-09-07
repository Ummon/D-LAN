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
  
#include <priv/Cache/DataWriter.h>
using namespace FM;

#include <QByteArray>
#include <QScopeGuard>

#include <Common/Settings.h>

#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Cache/DataReader.h>

/**
  * @remarks The setting "check_received_data_integrity" can be changed at runtime.
  * @exception UnableToOpenFileInReadModeException If the data already known of the chunk can't be read to check its integrity.
  * @exception IOErrorException
  * @exception ChunkDeletedException
  * @exception ChunkDataUnknownException
  */
DataWriter::DataWriter(Chunk& chunk) :
   CHECK_DATA_INTEGRITY(SETTINGS.get<bool>("check_received_data_integrity")), chunk(chunk)
{
   // Opening first recreates a missing file and resets all chunk byte counts.
   // Otherwise integrity checking tries to read data from the deleted file.
   this->chunk.newDataWriterCreated();
   auto rollback = qScopeGuard([this] { this->chunk.dataWriterDeleted(); });
   this->computeChunkHash();
   rollback.dismiss();
}

DataWriter::~DataWriter()
{
   this->chunk.dataWriterDeleted();
}

bool DataWriter::write(const char* buffer, int nbBytes)
{
   if (nbBytes < 0 || (nbBytes > 0 && buffer == nullptr))
      throw IOErrorException();

   if (this->CHECK_DATA_INTEGRITY)
   {
      // A failed write may have hashed bytes that were never committed to knownBytes.
      // Rebuild lazily so the original I/O exception is preserved, and retries use only accepted data.
      if (this->hashNeedsRebuild)
      {
         this->hasher.reset();
         this->computeChunkHash();
      }
      this->hashNeedsRebuild = true;
      this->hasher.addData(std::span<const char>(buffer, static_cast<size_t>(nbBytes)));
      if (qint64(this->chunk.getKnownBytes()) + nbBytes == this->chunk.getChunkSize() && this->hasher.getResult() != this->chunk.getHash())
      {
         this->chunk.setKnownBytes(0);
         throw hashMismatchException();
      }
   }

   const bool complete = this->chunk.write(buffer, nbBytes);
   this->hashNeedsRebuild = false;
   return complete;
}

/**
  * Compute the hash of the first known data of the current chunk ('this->chunk'), the result is held by 'this->hasher'.
  * If the data can't be read the exception is propagated: without it the final hash could never match and the
  * peer sending the rest of the chunk would be wrongly blamed for corrupted data.
  * @exception UnableToOpenFileInReadModeException
  */
void DataWriter::computeChunkHash()
{
   if (this->CHECK_DATA_INTEGRITY && this->chunk.getKnownBytes() > 0)
   {
      static const quint32 BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_reading");
      QByteArray buffer(BUFFER_SIZE, Qt::Uninitialized);

      DataReader reader(this->chunk);
      int offset = 0;
      int bytesRead = 0;

      while ((bytesRead = reader.read(buffer.data(), offset)))
      {
         if (bytesRead < 0 || bytesRead > buffer.size())
            throw IOErrorException();
         this->hasher.addData(std::span<const char>(buffer).first(static_cast<size_t>(bytesRead)));
         offset += bytesRead;
      }
   }
}
