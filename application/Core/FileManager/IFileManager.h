#ifndef FILEMANAGER_IFILEMANAGER_H
#define FILEMANAGER_IFILEMANAGER_H

#include <QStringList>
#include <QBitArray>
#include <QSharedPointer>

#include <Common/Hash.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

namespace FM
{
   class IChunk;
   class IGetHashesResult;

   class IFileManager : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IFileManager() {}

      /**
        * @exception DirNotFoundException
        */
      virtual void setSharedDirsReadOnly(const QStringList& dirs) = 0;

      /**
        * @exception DirNotFoundException
        */
      virtual void setSharedDirsReadWrite(const QStringList& dirs) = 0;

      virtual QStringList getSharedDirsReadOnly() = 0;
      virtual QStringList getSharedDirsReadWrite() = 0;

      /**
        * Returns a chunk. If no chunk is found return a empty pointer.
        */
      virtual QSharedPointer<IChunk> getChunk(const Common::Hash& hash) = 0;

      /**
        * Create a new empty file. It will be automatically create in the same path than the remote.
        * It will take the shared directory which has enought storage space and matches paths the closest.
        * The file will have the exact final size and filled with 0.
        * The filename will end with .unfinished.
        * Some or all hashes can be null (see Protos.Common.Hash). They can be set later with IChunk::setHash(..).
        * @remarks Entry.shared_dir is not used.
        * @exception NoReadWriteSharedDirectoryException
        * @exception InsufficientStorageSpaceException
        * @exception FilePhysicallyAlreadyExistsException
        * @exception UnableToCreateNewFileException
        */
      virtual QList< QSharedPointer<IChunk> > newFile(const Protos::Common::Entry& remoteEntry) = 0;

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

      virtual QBitArray haveChunks(const QList<Common::Hash>& hashes) = 0;

      virtual quint64 getAmount() = 0;

   signals:
      /**
        * Emitted when the file cache has been loaded.
        */
      void fileCacheLoaded();
   };
}
#endif
