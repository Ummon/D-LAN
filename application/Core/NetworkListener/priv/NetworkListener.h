#ifndef NETWORKMANAGER_NETWORKLISTENER_H
#define NETWORKMANAGER_NETWORKLISTENER_H

//The message IMAlive is sent each 10s.
#define IMAliveFrequency 0.1

#include <QList>
#include <QSharedPointer>
#include <QTimer>
#include <QObject>

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
          QTimer *timer;
          QSharedPointer<LogManager::ILogger> logger;

       public slots:
          void presence();
   };
}
#endif
