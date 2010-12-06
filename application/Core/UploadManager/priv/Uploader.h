#ifndef UPLOADMANAGER_UPLOADER_H
#define UPLOADMANAGER_UPLOADER_H

#include <QThread>
#include <QSharedPointer>
#include <QTimer>
#include <QMutex>

#include <Common/Uncopyable.h>
#include <Common/TransferRateCalculator.h>
#include <Core/FileManager/IChunk.h>
#include <Core/FileManager/IDataReader.h>
#include <Core/PeerManager/ISocket.h>

#include <IUpload.h>

namespace UM
{
   class Uploader : public QThread, public IUpload, Common::Uncopyable
   {
      Q_OBJECT
      static quint64 currentID;

   public:
      Uploader(QSharedPointer<FM::IChunk> chunk, int offset, QSharedPointer<PM::ISocket> socket);

      quint64 getID() const;
      int getUploadRate() const;
      Common::Hash getPeerID() const;
      int getProgress() const;
      QSharedPointer<FM::IChunk> getChunk() const;
      QSharedPointer<PM::ISocket> getSocket() const;
      void startTimer();

   signals:
      void uploadFinished(bool error);
      void uploadTimeout();

   protected:
      void run();

   private:
      const quint64 ID;
      QSharedPointer<FM::IChunk> chunk;
      int offset;
      QSharedPointer<PM::ISocket> socket;
      QTimer timer;
      mutable QMutex mutex;

      Common::TransferRateCalculator transferRateCalculator;

      QThread* mainThread;
   };
}
#endif
