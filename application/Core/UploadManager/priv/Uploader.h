#ifndef UPLOADMANAGER_UPLOADER_H
#define UPLOADMANAGER_UPLOADER_H

#include <QThread>
#include <QSharedPointer>

#include <Core/FileManager/IChunk.h>

#include <IUpload.h>

namespace PM { class ISocket; }

namespace UM
{
   class Uploader : public QThread, public IUpload
   {
   public:
      Uploader(QSharedPointer<FM::IChunk> chunk, int offset, PM::ISocket* socket);
      void run();

      Common::Hash getPeerID() const;
      QSharedPointer<FM::IChunk> getChunk() const;

   private:
      QSharedPointer<FM::IChunk> chunk;
      int offset;
      PM::ISocket* socket;
   };
}
#endif
