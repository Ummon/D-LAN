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
  
#ifndef FILEMANAGER_EXCEPTIONS_H
#define FILEMANAGER_EXCEPTIONS_H

#include <QStringList>

namespace FM
{
   class FileSystemEntriesNotFoundException
   {
   public :
      FileSystemEntriesNotFoundException(const QStringList& paths) : paths(paths) {}
      virtual ~FileSystemEntriesNotFoundException() throw () {}
      const QStringList paths;
   };

   class FilesNotFoundException : public FileSystemEntriesNotFoundException
   {
   public :
      FilesNotFoundException(const QStringList& paths) : FileSystemEntriesNotFoundException(paths) {}
      virtual ~FilesNotFoundException() throw () {}
   };

   class ItemsNotFoundException : public FileSystemEntriesNotFoundException
   {
   public :
      ItemsNotFoundException(const QStringList& paths) : FileSystemEntriesNotFoundException(paths) {}
      virtual ~ItemsNotFoundException() throw () {}
   };

   class hashMissmatchException {};

   class NoWriteableDirectoryException{};

   class ScanningException{};

   class IOErrorException {};

   class FileResetException {};

   class UnableToOpenFileInWriteModeException {};

   class UnableToOpenFileInReadModeException {};

   class TryToWriteBeyondTheEndOfChunkException {};

   class SharedEntryAlreadySharedException {};

   class ChunkDeletedException {};

   class ChunkDataUnknownException {};

   class InsufficientStorageSpaceException {};

   class UnableToCreateNewFileException {};

   class UnableToCreateNewDirException {};

   class UnableToCreateSharedDirectory {};
}

#endif
