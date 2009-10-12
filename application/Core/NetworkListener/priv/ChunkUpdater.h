#ifndef NETWORKMANAGER_CHUNKUPDATER_H
#define NETWORKMANAGER_CHUNKUPDATER_H

#include <QtGlobal>

namespace DownloadManager { class IDownloadManager; }

namespace NetworkListener
{
    class UDPListener;
    class ChunkUpdater
    {
        private:
            DownloadManager::IDownloadManager* downloadManager;
            quint32 currentTag; ///< The tag of the last sent 'HaveChunks' message.
            UDPListener* udpListener;
    };
}
#endif
