#ifndef BUILDER_BUILDER__H
#define BUILDER_BUILDER__H

#include <QTextStream>
#include <QSharedPointer>

namespace PM
{
   class IPeerManager;

   class Builder
   {
      public:
         static QSharedPointer<IPeerManager> newPeerManager();
   };
}
#endif
