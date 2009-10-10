#ifndef NETWORKLISTENER_BUILDER_H
#define NETWORKLISTENER_BUILDER_H

#include <QTextStream>
#include <QSharedPointer>

#include "NetworkListener_global.h"

namespace NetworkListener
{
   class INetworkListener;
   class IChat;
   class ISearch;

   class NETWORKLISTENERSHARED_EXPORT Builder
   {
   public:

      static QSharedPointer<INetworkListener> newNetworkListener();
   };
}
#endif
