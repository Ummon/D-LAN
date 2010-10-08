#ifndef UPLOADMANAGER_UPLOADMANAGER_H
#define UPLOADMANAGER_UPLOADMANAGER_H

#include <QSharedPointer>
#include <QList>

#include <Common/Hash.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>

#include <IUploadManager.h>
#include <priv/Uploader.h>

namespace UM
{
   class Upload;

   class UploadManager : public QObject, public IUploadManager
   {
      Q_OBJECT
   public:
      UploadManager(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager);

   private slots:
      void getChunk(Common::Hash hash, int offset, PM::ISocket* socket);

   private:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;

      QList< QSharedPointer<Uploader> > uploaders;
   };
}
#endif
