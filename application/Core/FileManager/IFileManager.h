#ifndef FILEMANAGER_IFILEMANAGER_H
#define FILEMANAGER_IFILEMANAGER_H

#include <QStringList>
#include <QBitArray>
#include <QSharedPointer>

#include <Common/Hash.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

namespace FM
{
   class IChunk;
   class IFile;
   class IGetHashesResult;

   class IFileManager : public QObject // These are not the droids you are looking for.
   {
   public:
      virtual ~IFileManager() {}

      virtual QStringList getSharedDirsReadOnly() = 0;
      virtual QStringList getSharedDirsReadWrite() = 0;

      /**
        * @exception DirNotFoundException
        */
      virtual void setSharedDirsReadOnly(const QStringList& dirs) = 0;

      /**
        * @exception DirNotFoundException
        */
      virtual void setSharedDirsReadWrite(const QStringList& dirs) = 0;

      virtual IChunk* getChunk(const Common::Hash& hash) = 0;

      // virtual IGetHashesResult* getHashes(const Protos::Common::FileEntry& entry) = 0;
      // virtual Protos::Core::GetEntriesResult* getEntries(const Protos::Common::DirEntry& entry) = 0;
      virtual Protos::Common::FindResult find(const QString& words) = 0;
      virtual QBitArray haveChunks(const QList<Common::Hash>& hashes) = 0;
      virtual quint64 getAmount() = 0;
      virtual QSharedPointer<IFile> newFile(const Protos::Common::FileEntry& remoteEntry) = 0;
   };
}
#endif
