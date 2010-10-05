#include <priv/DirDownload.h>
using namespace DM;

DirDownload::DirDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager>, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : Download(fileManager, peerManager, peerSourceID, entry)
{
}
