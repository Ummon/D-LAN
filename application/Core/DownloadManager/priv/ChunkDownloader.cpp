#include <priv/ChunkDownloader.h>
using namespace DM;

#include <Core/PeerManager/IPeer.h>

ChunkDownloader::ChunkDownloader()
   : idle(true)
{
}

void ChunkDownloader::run()
{
   this->exec();
}

bool ChunkDownloader::isIdle() const
{
   return this->idle;
}

Common::Hash ChunkDownloader::getHash()
{
   // TODO
   return Common::Hash();
}

void ChunkDownloader::setPeerIDs(const QList<Common::Hash>& peerIDs)
{
   // TODO
}

//void ChunkDownloader::chunkReadyToDownload(QSharedPointer<ChunkDownload> chunkDownload)
//{
//   chunkDownload->downloadChunk();
//}
