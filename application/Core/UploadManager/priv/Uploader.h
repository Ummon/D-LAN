#ifndef UPLOADMANAGER_UPLOADER_H
#define UPLOADMANAGER_UPLOADER_H

#include <QThread>
#include <QSharedPointer>

#include <Common/TransferRateCalculator.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/IDataReader.h>

#include <IUpload.h>

namespace PM { class ISocket; }

namespace UM
{
   class Uploader : public QThread, public IUpload
   {
      Q_OBJECT
   public:
      Uploader(QSharedPointer<FM::IChunk> chunk, int offset, PM::ISocket* socket);

      int getUploadRate() const;

      Common::Hash getPeerID() const;

      int getProgress() const;

      QSharedPointer<FM::IChunk> getChunk() const;

      PM::ISocket* getSocket();

   signals:
      void uploadFinished(bool error);

   protected:
      void run();

   private:
      QSharedPointer<FM::IChunk> chunk;
      int offset;
      PM::ISocket* socket;

      Common::TransferRateCalculator transferRateCalculator;

      QThread* mainThread;
   };
}
#endif
