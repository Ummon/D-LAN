#ifndef UPLOADMANAGER_UPLOAD_H
#define UPLOADMANAGER_UPLOAD_H

#include <QMutex>
#include <QThread>

#include <Common/Timeoutable.h>
#include <Common/TransferRateCalculator.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/IDataReader.h>
#include <Core/PeerManager/ISocket.h>

#include <IUpload.h>

namespace UM
{
   class Upload : public IUpload, public Common::Timeoutable
   {
      static quint64 currentID; ///< Used to generate the new upload ID.

   public:
      Upload(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket, Common::TransferRateCalculator& transferRateCalculator);
      ~Upload();

      quint64 getID() const;
      Common::Hash getPeerID() const;
      int getProgress() const;
      QSharedPointer<FM::IChunk> getChunk() const;

      void setAsFinished();
      void moveSocketToThread(QThread* thread);

      void upload();
      void stop();

   private:
      mutable QMutex mutex;
      bool toStop;

      const quint64 ID; ///< Each uploader has an ID to identified it.
      QSharedPointer<FM::IChunk> chunk; ///< The chunk uploaded.
      int offset; ///< The current offset into the chunk.
      QSharedPointer<PM::ISocket> socket;

      Common::TransferRateCalculator& transferRateCalculator;

      bool networkError;
   };
}

#endif
