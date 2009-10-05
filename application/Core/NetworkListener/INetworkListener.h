#ifndef NETWORKMANAGER_INETWORKLISTENER_H
#define NETWORKMANAGER_INETWORKLISTENER_H

namespace NetworkListener
{
   class IChat;
   class ISearch;

   class INetworkListener
   {
   public:
      IChat* getChat();
      ISearch* search();
   };
}
#endif
