#ifndef NETWORKMANAGER_NETWORKLISTENER_H
#define NETWORKMANAGER_NETWORKLISTENER_H

#include <QList>

#include <INetworkListener.h>

namespace NetworkListener
{
    class ChunkUpdater;
    class TCPListener;
    class UDPListener;
    class Chat;
    class Search;

   class NetworkListener : public INetworkListener
   {
   private:
      ChunkUpdater* chunkUpdater;
      TCPListener* tcpListener;
      UDPListener* udpListener ;
      Chat* chat;
      QList<Search*> searches;
   };
}
#endif
