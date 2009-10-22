#ifndef FILEMANAGER_CHUNK_H
#define FILEMANAGER_CHUNK_H

#include <exception>

#include <QByteArray>

#include <Common/Hash.h>
#include <IChunk.h>
#include <priv/Cache/File.h>

namespace FM
{
   class ChunkNotCompletedException : public std::exception {};

   class IDataReader;
   class IDataWriter;

   class Chunk : public IChunk
   {
   public:
      Chunk(File& file, const Common::Hash& hash, int num, int knownBytes = File::CHUNK_SIZE);
      virtual ~Chunk() {};

      QSharedPointer<IDataReader> getDataReader();
      QSharedPointer<IDataWriter> getDataWriter();

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

      void sendContentToSocket(QAbstractSocket& socket);
      void getContentFromSocket(QAbstractSocket& socket);

      Common::Hash getHash();

      int getKnownBytes();

      File& getFile();

   private:
      File& file;
      Common::Hash hash;
      int num;
      int knownBytes; ///< Relative offset, 0 means we don't have any byte and File::CHUNK_SIZE means we have all the chunk data.
   };
}
#endif
