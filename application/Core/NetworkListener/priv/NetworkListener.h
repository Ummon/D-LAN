#ifndef NETWORKMANAGER_NETWORKLISTENER_H
#define NETWORKMANAGER_NETWORKLISTENER_H

#include <QList>
#include <QSharedPointer>

#include <Common/LogManager/ILogger.h>

#include <INetworkListener.h>
#include <priv/UDPListener.h>

namespace NetworkListener
{
    class ChunkUpdater;
    class TCPListener;
    class UDPListener;
    class Chat;
    class Search;

   class NetworkListener : public INetworkListener {
       public:
           NetworkListener();
           IChat* getChat();

       private:
          ChunkUpdater* chunkUpdater;
          TCPListener* tcpListener;
          UDPListener* udpListener ;
          Chat* chat;
          QList<Search*> searches;

          QSharedPointer<LogManager::ILogger> logger;
   };
}
#endif
