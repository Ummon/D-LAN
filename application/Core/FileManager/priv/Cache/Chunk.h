#ifndef FILEMANAGER_CHUNK_H
#define FILEMANAGER_CHUNK_H

#include <exception>

#include <QByteArray>
#include <QMutex>

#include <Protos/files_cache.pb.h>
#include <Common/Hash.h>
#include <Common/Uncopyable.h>
#include <IChunk.h>
#include <priv/Constants.h>

namespace FM
{
   class File;
   class IDataReader;
   class IDataWriter;

   class Chunk : public IChunk, Common::Uncopyable
   {
   public:
      /**
        * Create a new empty chunk.
        */
      Chunk(File* file, int num, quint32 knownBytes);
      Chunk(File* file, int num, quint32 knownBytes, const Common::Hash& hash);

   public:
      ~Chunk();

      void removeItsFile();

      Chunk* restoreFromFileCache(const Protos::FileCache::Hashes_Chunk& chunk);

      void populateHashesChunk(Protos::FileCache::Hashes_Chunk& chunk) const;

      void populateEntry(Protos::Common::Entry* entry) const;

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
      int read(char* buffer, int offset);

      /**
        * @return 'true' if end of chunk reached.
        */
      bool write(const char* buffer, int nbBytes);

      //void sendContentToSocket(QAbstractSocket& socket);
      //void getContentFromSocket(QAbstractSocket& socket);

      int getNum() const;

      int getNbTotalChunk() const;

      QList< QSharedPointer<Chunk> > getOtherChunks() const;

      bool hasHash() const;

      Common::Hash getHash() const;

      void setHash(const Common::Hash& hash);

      int getKnownBytes() const;
      void setKnownBytes(int bytes);

      int getChunkSize() const;

      bool isComplete() const;

      bool isOwnedBy(File* file) const;

      QString toStr() const;

   private:
      const int CHUNK_SIZE;
      const int BUFFER_SIZE;

      mutable QMutex mutex; ///< Protect 'file' against multiple access.

      File* file;
      int num;
      int knownBytes; ///< Relative offset, 0 means we don't have any byte and CHUNK_SIZE means we have all the chunk data.
      Common::Hash hash;
   };
}
#endif
