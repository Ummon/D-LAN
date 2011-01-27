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
  
#ifndef FILEMANAGER_IFILEMANAGER_H
#define FILEMANAGER_IFILEMANAGER_H

#include <QStringList>
#include <QBitArray>
#include <QSharedPointer>

#include <Common/Hash.h>
#include <Common/Hashes.h>
#include <Common/SharedDir.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

namespace FM
{
   class IChunk;
   class IGetHashesResult;

   /**
     * The file manager controls all shared directories and files. It offers these fonctions :
     * - Add or remove one ore more shared directory. A directory can be have read or read-write rights.
     * - Watch the shared directories recursively to update the model if a file/directory is addes, changed, renamed or removed.
     * - Browse the cache.
     * - Offer a quick indexed multi-term search based on the names of files and directories.
     * - Offer a way to indentify each chunk of a file by a hash.
     * - Read or write a file chunk.
     * - Persist data to avoid re-hashing.
     */
   class IFileManager : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IFileManager() {}

      /**
        * @exception DirsNotFoundException
        */
      virtual void setSharedDirs(const QStringList& dirs) = 0;

      virtual QList<Common::SharedDir> getSharedDirs() const = 0;

      /**
        * Returns the absolute path to the corresponding shared directory.
        * If the directory is not found an empty string is returned.
        */
      virtual QString getSharedDir(const Common::Hash& ID) const = 0;

      /**
        * Returns a chunk. If no chunk is found return a empty pointer.
        */
      virtual QSharedPointer<IChunk> getChunk(const Common::Hash& hash) const = 0;

      /**
        * Get all chunks from the file which owns the given hashes.
        * The name and the path of the owner of the returned chunk must match the given entry.
        * ".unfinished" suffix is ignored in both side.
        */
      virtual QList< QSharedPointer<IChunk> > getAllChunks(const Protos::Common::Entry& localEntry, const Common::Hashes& hashes) const = 0;

      /**
        * Create a new empty file.
        * It will take the shared directory which has enought storage space and matches paths the closest.
        * The file will have the exact final size and filled with 0.
        * The filename will end with ".unfinished".
        * Some or all hashes can be null (see Protos.Common.Hash). They can be set later with IChunk::setHash(..).
        * @exception NoWriteableDirectoryException
        * @exception InsufficientStorageSpaceException
        * @exception UnableToCreateNewFileException
        */
      virtual QList< QSharedPointer<IChunk> > newFile(Protos::Common::Entry& entry) = 0;

      /**
        * Return the hashes from a FileEntry. If the hashes don't exist they will be computed on the fly. However this
        * Method is non-blocking, when the hashes are ready a signal will be emited by the IGetHashesResult object.
        */
      virtual QSharedPointer<IGetHashesResult> getHashes(const Protos::Common::Entry& file) = 0;

      /**
        * Returns the directories and files contained in the given directory.
        */
      virtual Protos::Common::Entries getEntries(const Protos::Common::Entry& dir) = 0;

      /**
        * Returns the shared directories (roots).
        */
      virtual Protos::Common::Entries getEntries() = 0;

      /**
        * Find some entry from a given words.
        * @param maxSize This is the size in bytes each 'FindResult' can't exceed. (Because UDP datagrams have a maximum size).
        * It should not be here but it's far more harder to split the result outside this method.
        * @remarks Will not fill the fields 'FindResult.tag' and 'FindResult.peer_id'.
        */
      virtual QList<Protos::Common::FindResult> find(const QString& words, int maxNbResult, int maxSize) = 0;

      /**
        * Ask if we have the given hashes. For each hashes a bit is set (1 if the hash is known or 0 otherwise) into the returned QBitArray.
        */
      virtual QBitArray haveChunks(const QList<Common::Hash>& hashes) = 0;

      /**
        * Return the amount of shared data.
        */
      virtual quint64 getAmount() = 0;

      enum CacheStatus {
         SCANNING_IN_PROGRESS = 0,
         HASHING_IN_PROGRESS = 1,
         UP_TO_DATE = 2
      };

      virtual CacheStatus getCacheStatus() const = 0;

   signals:
      /**
        * Emitted when the file cache has been loaded.
        */
      void fileCacheLoaded();
   };
}
#endif
