#include <Builder.h>
using namespace NetworkListener;

#include <INetworkListener.h>
#include <IChat.h>
#include <ISearch.h>

#include <priv/NetworkListener.h>
#include <priv/Chat.h>
#include <priv/Search.h>

QSharedPointer<INetworkListener> Builder::newNetworkListener()
{
   return QSharedPointer<INetworkListener>(new NetworkListener());
}

QSharedPointer<IChat> Builder::newChat()
{
   return QSharedPointer<IChat>(new Chat());
}

QSharedPointer<ISearch> Builder::newSearch()
{
   return QSharedPointer<ISearch>(new Search());
}
