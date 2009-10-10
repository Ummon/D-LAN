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
          Chat();
          virtual ~Chat() {}
          void send(const QString& message);

       private:
          UDPListener* udpListener;
          QSharedPointer<LogManager::ILogger> logger;
   };
}
#endif
