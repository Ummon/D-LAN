#include <priv/FileDownload.h>
using namespace DM;

FileDownload::FileDownload(QSharedPointer<FM::IFileManager> fileManager, QSharedPointer<PM::IPeerManager>, Common::Hash peerSourceID, const Protos::Common::Entry& entry)
   : Download(fileManager, peerManager, peerSourceID, entry)
{

}
