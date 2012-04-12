#ifndef PEERMANAGER_PEERSELF_H
#define PEERMANAGER_PEERSELF_H

#include <priv/Peer.h>

namespace PM
{
   class PeerSelf : public Peer
   {
   public:
      PeerSelf(PeerManager* peerManager, QSharedPointer<FM::IFileManager> fileManager);

      void setNick(const QString& nick);

   private:
      static Common::Hash loadID();
   };
}

#endif
