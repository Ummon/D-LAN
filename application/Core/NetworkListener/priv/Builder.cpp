#include <Builder.h>
using namespace NetworkListener;

#include <INetworkListener.h>
#include <IChat.h>
#include <ISearch.h>

#include <priv/NetworkListener.h>
#include <priv/Chat.h>
#include <priv/Search.h>

/**
 * Return a new instante of a NetworkListener
 *
 * @author mcuony
 */
QSharedPointer<INetworkListener> Builder::newNetworkListener()
{
   return QSharedPointer<INetworkListener>(new NetworkListener());
}
