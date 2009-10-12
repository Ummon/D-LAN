#ifndef BUILDER_BUILDER__H
#define BUILDER_BUILDER__H

#include <QTextStream>
#include <QSharedPointer>

#include "PeerManager_global.h"

namespace PeerManager
{
   class IPeerManager;


   class PEERMANAGERSHARED_EXPORT Builder
   {
       public:

          static QSharedPointer<IPeerManager> newPeerManager();
   };
}
#endif
