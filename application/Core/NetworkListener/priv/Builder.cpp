#include <Builder.h>
using namespace NetworkListener;

#include <INetworkListener.h>

#include <priv/NetworkListener.h>

/**
 * Return a new instante of a NetworkListener
 *
 * @author mcuony
 */
QSharedPointer<INetworkListener> Builder::newNetworkListener(QSharedPointer<PeerManager::IPeerManager> peerManager)
{
   return QSharedPointer<INetworkListener>(new NetworkListener(peerManager));
}
