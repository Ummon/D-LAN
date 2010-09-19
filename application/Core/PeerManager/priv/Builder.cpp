#include <Builder.h>
using namespace PM;

#include <IPeerManager.h>
#include <priv/PeerManager.h>

/**
 * Return a new instante of a PeerManager
 */
QSharedPointer<IPeerManager> Builder::newPeerManager(QSharedPointer<FM::IFileManager> fileManager)
{
   return QSharedPointer<IPeerManager>(new PeerManager(fileManager));
}
