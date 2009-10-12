#ifndef NETWORKMANAGER_SEARCH_H
#define NETWORKMANAGER_SEARCH_H

#include <ISearch.h>

namespace NetworkListener { class UDPListener; }

namespace NetworkListener
{
   class UDPListener;

   class Search : public ISearch
   {
       private:
          quint32 tag;
          UDPListener* udpListener;
   };
}
#endif
