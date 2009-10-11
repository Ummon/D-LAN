#ifndef NETWORKLISTENER_BUILDER_H
#define NETWORKLISTENER_BUILDER_H

#include <QTextStream>
#include <QSharedPointer>

#include "NetworkListener_global.h"
#include <Core/PeerManager/IPeerManager.h>

namespace NetworkListener
{
   class INetworkListener;

   class NETWORKLISTENERSHARED_EXPORT Builder
   {
   public:

      static QSharedPointer<INetworkListener> newNetworkListener(QSharedPointer<PeerManager::IPeerManager>);
   };
}
#endif
