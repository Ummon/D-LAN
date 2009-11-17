#ifndef FILEMANAGER_CHUNK_H
#define FILEMANAGER_CHUNK_H

#include <exception>

#include <QByteArray>
#include <QMutex>

#include <Protos/files_cache.pb.h>
#include <Common/Hash.h>
#include <IChunk.h>
#include <priv/Constants.h>

namespace FM
{
   class ChunkNotCompletedException : public std::exception {};

   class File;
   class Cache;
   class IDataReader;
   class IDataWriter;

   class Chunk : public IChunk
   {
   public:
      /**
        * Create a new empty chunk.
        */
      Chunk(Cache* cache, File* file, int num, int knownBytes);

      Chunk(Cache* cache, File* file, int num, int knownBytes, const Common::Hash& hash);

      virtual ~Chunk();

      Chunk* restoreFromFileCache(const Protos::FileCache::Hashes_Chunk& chunk);

      void populateHashesChunk(Protos::FileCache::Hashes_Chunk& chunk);

      QSharedPointer<IDataReader> getDataReader();
      QSharedPointer<IDataWriter> getDataWriter();

      void newDataWriterCreated();
      void newDataReaderCreated();

      void dataWriterDeleted();
      void dataReaderDeleted();

      /**
        * Called by a deleted file just before dying.
        */
      void fileDeleted();

      /**
        * Fill the given buffer with read bytes.
        *
        * @param buffer The buffer.
        * @param offset The offset relative to the chunk.
        * @return The number of read bytes. If lesser than 'buffer.size' the end of file has been reached
        *         and the buffer will be partially filled.
        * @exception ChunkNotCompletedException
        */
      int read(QByteArray& buffer, int offset);

      /**
        * @return 'true' if end of file reached.
        */
      bool write(const QByteArray& buffer, int offset);

      //void sendContentToSocket(QAbstractSocket& socket);
      //void getContentFromSocket(QAbstractSocket& socket);

      bool hasHash();

      Common::Hash getHash();

      void setHash(const Common::Hash& hash);

      int getKnownBytes();

      /*File& getFile();*/

   private:
      QMutex mutex; ///< Protect 'file' against multiple access.

      Cache* cache;
      File* file;
      int num;
      int knownBytes; ///< Relative offset, 0 means we don't have any byte and File::CHUNK_SIZE means we have all the chunk data.
      Common::Hash hash;
   };
}
#endif
