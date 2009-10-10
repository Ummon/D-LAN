#ifndef NETWORKMANAGER_CHAT_H
#define NETWORKMANAGER_CHAT_H

#include <IChat.h>
#include <Common/LogManager/ILogger.h>
#include <QSharedPointer>
#include <Protos/core_protocol.pb.h>

namespace NetworkListener
{
   class UDPListener;

   class Chat : public IChat
   {
       public:
          Chat(UDPListener* udpListener_);
          virtual ~Chat() {}
          void send(const QString& message);

       private:
          UDPListener* udpListener;
          QSharedPointer<LogManager::ILogger> logger;

       public slots:
            void newChatMessage(const Protos::Core::ChatMessage& message);
   };
}
#endif
