#include <Builder.h>
using namespace PM;

#include <IPeerManager.h>


#include <priv/PeerManager.h>


/**
 * Return a new instante of a PeerManager
 *
 * @author mcuony
 */
QSharedPointer<IPeerManager> Builder::newPeerManager()
{
   return QSharedPointer<IPeerManager>(new PeerManager());
}
