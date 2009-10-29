#include <Builder.h>
using namespace NL;

#include <INetworkListener.h>

#include <priv/NetworkListener.h>

/**
 * Return a new instante of a NetworkListener
 *
 * @author mcuony
 */
QSharedPointer<INetworkListener> Builder::newNetworkListener(QSharedPointer<PM::IPeerManager> peerManager)
{
   return QSharedPointer<INetworkListener>(new NetworkListener(peerManager));
}
