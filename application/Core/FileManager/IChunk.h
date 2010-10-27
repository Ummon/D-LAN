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
        * The caller must not delete the IChunk as long as data is read with the IDataReader.
        * @exception UnableToOpenFileInReadMode
        */
      virtual QSharedPointer<IDataReader> getDataReader() = 0;

      /**
        * The caller must not delete the IChunk as long as data is written with the IDataWriter.
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

      virtual int getNbTotalChunk() const = 0;

      virtual Common::Hash getHash() const = 0;

      virtual void setHash(const Common::Hash&) = 0;

      virtual int getKnownBytes() const = 0;

      virtual int getChunkSize() const = 0;

      virtual bool isComplete() const = 0;

      virtual QString toStr() const = 0;
   };
}
#endif
