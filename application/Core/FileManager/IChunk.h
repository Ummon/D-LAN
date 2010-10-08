#ifndef FILEMANAGER_ICHUNK_H
#define FILEMANAGER_ICHUNK_H

#include <QSharedPointer>

#include <Protos/common.pb.h>

#include <Common/Hash.h>

namespace FM
{
   class IDataReader;
   class IDataWriter;

   class IChunk
   {
   public:
      virtual ~IChunk() {}

      virtual void removeItsFile() = 0;

      virtual void populateEntry(Protos::Common::Entry* entry) const = 0;

      /**
        * @exception UnableToOpenFileInReadMode
        */
      virtual QSharedPointer<IDataReader> getDataReader() = 0;

      /**
        * @exception UnableToOpenFileInWriteMode
        */
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

      virtual int getNum() const = 0;

      virtual Common::Hash getHash() const = 0;

      virtual void setHash(const Common::Hash&) = 0;

      virtual quint32 getKnownBytes() const = 0;
   };
}
#endif
