#ifndef NETWORKLISTENER_BUILDER_H
#define NETWORKLISTENER_BUILDER_H

#include <QTextStream>
#include <QSharedPointer>

#include <PeerManager/IPeerManager.h>

namespace NL
{
   class INetworkListener;

   class Builder
   {
   public:
      static QSharedPointer<INetworkListener> newNetworkListener(QSharedPointer<PM::IPeerManager>);
   };
}
#endif
