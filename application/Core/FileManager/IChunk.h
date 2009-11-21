#ifndef FILEMANAGER_ICHUNK_H
#define FILEMANAGER_ICHUNK_H

#include <QSharedPointer>

#include <Common/Hash.h>

namespace FM
{
   class IDataReader;
   class IDataWriter;

   class IChunk
   {
   public:
      virtual ~IChunk() {}

      virtual QSharedPointer<IDataReader> getDataReader() = 0;
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

      virtual Common::Hash getHash() = 0;

      virtual void setHash(const Common::Hash&) = 0;

      virtual int getKnownBytes() = 0;
   };
}
#endif
