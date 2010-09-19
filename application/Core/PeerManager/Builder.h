#ifndef PEERMANAGER_BUILDER__H
#define PEERMANAGER_BUILDER__H

#include <QTextStream>
#include <QSharedPointer>

#include <Core/FileManager/IFileManager.h>

namespace PM
{
   class IPeerManager;

   class Builder
   {
      public:
         static QSharedPointer<IPeerManager> newPeerManager(QSharedPointer<FM::IFileManager> fileManager);
   };
}
#endif
