#ifndef DOWNLOADMANAGER_DIRDOWNLOAD_H
#define DOWNLOADMANAGER_DIRDOWNLOAD_H

#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IGetEntriesResult.h>

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>

#include <priv/Download.h>

namespace DM
{
   class DirDownload : public Download
   {
      Q_OBJECT
   public:
      DirDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager> peerManager, Common::Hash peerSourceID, const Protos::Common::Entry& entry);

      void retrieveEntries();

   signals:
      void newEntries(const Protos::Core::GetEntriesResult& entries);

   private slots:
      void result(const Protos::Core::GetEntriesResult& entries);

   private:
      QSharedPointer<PM::IGetEntriesResult> getEntriesResult;;
   };
}
#endif
