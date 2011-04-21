/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#include <priv/Cache/DataWriter.h>
using namespace FM;

#include <Common/Settings.h>

#include <Exceptions.h>
#include <priv/Log.h>
#include <priv/Cache/DataReader.h>

/**
  * @remarks The setting "check_received_data_integrity" can be changed at runtime.
  */
DataWriter::DataWriter(Chunk& chunk) :
   CHECK_DATA_INTEGRITY(SETTINGS.get<bool>("check_received_data_integrity")), chunk(chunk)
{
   if (this->CHECK_DATA_INTEGRITY && this->chunk.getKnownBytes() > 0)
   {
      static const QString errorMessage("Unable to read chunk to check data integrity: %1");
      try
      {
         static const quint32 BUFFER_SIZE = SETTINGS.get<quint32>("buffer_size_reading");
         char buffer[BUFFER_SIZE];

         DataReader reader(this->chunk);
         int offset = 0;
         int bytesRead = 0;

         while (bytesRead = reader.read(buffer, offset))
         {
            this->hasher.addData(buffer, bytesRead);
            offset += bytesRead;
         }
      }
      catch(UnableToOpenFileInReadModeException&)
      {
         L_WARN(errorMessage.arg("UnableToOpenFileInReadModeException"));
      }
      catch(IOErrorException&)
      {
         L_WARN(errorMessage.arg("IOErrorException"));
      }
      catch(ChunkDeletedException&)
      {
         L_WARN(errorMessage.arg("ChunkDeletedException"));
      }
      catch(ChunkNotCompletedException&)
      {
         L_WARN(errorMessage.arg("ChunkNotCompletedException"));
      }
   }

   this->chunk.newDataWriterCreated();
}

DataWriter::~DataWriter()
{
   this->chunk.dataWriterDeleted();
}

bool DataWriter::write(const char* buffer, int nbBytes)
{
   if (this->CHECK_DATA_INTEGRITY)
   {
      this->hasher.addData(buffer, nbBytes);
      if (this->chunk.getKnownBytes() + nbBytes == this->chunk.getChunkSize() && this->hasher.getResult() != this->chunk.getHash())
      {
         this->chunk.setKnownBytes(0);
         throw hashMissmatchException();
      }
   }

   return this->chunk.write(buffer, nbBytes);
}
