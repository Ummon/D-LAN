#ifndef NETWORKMANAGER_UDPLISTENER_H
#define NETWORKMANAGER_UDPLISTENER_H

#include <QObject>

#include <Protos/core_protocol.pb.h>

namespace NetworkListener
{
   class UDPListener : QObject
   {
      Q_OBJECT
   signals:
      void newChatMessage(const Protos::Core::ChatMessage& message);
      void newFindResult(const Protos::Common::FindResult& result, const quint32& IP);
      void newHaveChunksResult(const Protos::Core::HaveChunksResult& result);
   };
}
#endif
