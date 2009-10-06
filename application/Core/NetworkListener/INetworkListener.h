#ifndef NETWORKMANAGER_INETWORKLISTENER_H
#define NETWORKMANAGER_INETWORKLISTENER_H

namespace NetworkListener
{
   class IChat;
   class ISearch;

   class INetworkListener
   {
   public:
      virtual ~INetworkListener() {}

      virtual IChat* getChat() = 0;
      virtual ISearch* search() = 0;
   };
}
#endif
