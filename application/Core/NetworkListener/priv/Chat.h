#ifndef NETWORKMANAGER_CHAT_H
#define NETWORKMANAGER_CHAT_H

#include <IChat.h>
#include <Common/LogManager/ILogger.h>
#include <QSharedPointer>
#include <Protos/core_protocol.pb.h>
#include <Core/PeerManager/IPeerManager.h>

namespace NL
{
   class UDPListener;

   class Chat : public IChat
   {
      Q_OBJECT
   public:
      Chat(UDPListener* newUudpListener, QSharedPointer<PM::IPeerManager> newPeerManager);
      virtual ~Chat() {}
      bool send(const QString& message);

   private:
      UDPListener* udpListener;
      QSharedPointer<LM::ILogger> logger;
      QSharedPointer<PM::IPeerManager> peerManager;

   public slots:
      void newChatMessage(const Protos::Core::ChatMessage& message);
   };
}
#endif
