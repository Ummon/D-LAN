#ifndef NETWORKMANAGER_CHUNKUPDATER_H
#define NETWORKMANAGER_CHUNKUPDATER_H

#include <QtGlobal>

namespace DM { class IDownloadManager; }

namespace NL
{
   class UDPListener;
   class ChunkUpdater
   {
   private:
      DM::IDownloadManager* downloadManager;
      quint32 currentTag; ///< The tag of the last sent 'HaveChunks' message.
      UDPListener* udpListener;
   };
}
#endif
