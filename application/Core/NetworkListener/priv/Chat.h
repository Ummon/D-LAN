#ifndef NETWORKMANAGER_CHAT_H
#define NETWORKMANAGER_CHAT_H

#include <IChat.h>

namespace NetworkListener
{
   class UDPListener;

   class Chat : public IChat
   {
   private:
      UDPListener* udpListener;
   };
}
#endif
