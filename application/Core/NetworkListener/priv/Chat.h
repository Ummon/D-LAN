#ifndef NETWORKMANAGER_CHAT_H
#define NETWORKMANAGER_CHAT_H

#include <IChat.h>
#include <Common/LogManager/ILogger.h>
#include <QSharedPointer>
#include <Protos/core_protocol.pb.h>
#include <Core/PeerManager/IPeerManager.h>

namespace NetworkListener
{
   class UDPListener;

   class Chat : public IChat
   {
      public:
         Chat(UDPListener* newUudpListener, QSharedPointer<PeerManager::IPeerManager> newPeerManager);
         virtual ~Chat() {}
         bool send(const QString& message);

      private:
         UDPListener* udpListener;
         QSharedPointer<LogManager::ILogger> logger;
         QSharedPointer<PeerManager::IPeerManager> peerManager;

      public slots:
         void newChatMessage(const Protos::Core::ChatMessage& message);
   };
}
#endif
