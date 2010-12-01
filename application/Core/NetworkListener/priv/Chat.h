#ifndef NETWORKLISTENER_CHAT_H
#define NETWORKLISTENER_CHAT_H

#include <QSharedPointer>

#include <Protos/core_protocol.pb.h>

#include <Common/Uncopyable.h>

#include <IChat.h>
#include <priv/UDPListener.h>

namespace NL
{
   class Chat : public IChat, Common::Uncopyable
   {
      Q_OBJECT
   public:
      Chat(UDPListener& uDPListener);
      virtual ~Chat() {}
      void send(const QString& message);

   private:
      UDPListener& uDPListener;
   };
}
#endif
