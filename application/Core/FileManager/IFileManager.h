#ifndef FILEMANAGER_IFILEMANAGER_H
#define FILEMANAGER_IFILEMANAGER_H

#include <QStringList>

#include <Common/Hash.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

namespace FileManager
{
   class IChunk;
   class IFile;
   class IGetHashesResult;

   class IFileManager
   {
   public:
      virtual ~IFileManager() {}

      virtual QStringList getSharedDirsReadOnly() = 0;
      virtual QStringList getSharedDirsReadWrite() = 0;
      virtual void setSharedDirsReadOnly(const QStringList& dirs) = 0;
      virtual void setSharedDirsReadWrite(const QStringList& dirs) = 0;

     virtual IChunk* getChunk(const Common::Hash& hash) = 0;

      /*virtual IGetHashesResult* getHashes(const  Protos::Common::FileEntry& entry) = 0;
      virtual Protos::Core::GetEntriesResult* getEntries(const Protos::Common::DirEntry& entry) = 0;*/
      virtual Protos::Common::FindResult find(const QString& words) = 0;
      /*virtual QList<bool> haveChunks(const QList<Common::Hash>& hashes) = 0;
      virtual quint64 getAmount() = 0;
      virtual IFile newFile(const Protos::Common::FileEntry& remotEntry) = 0;*/
   };
}
#endif
