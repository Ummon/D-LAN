#ifndef NETWORKLISTENER_UTILS_H
#define NETWORKLISTENER_UTILS_H

#include <QHostAddress>

namespace NL
{
   class Utils
   {
   public:
      static QHostAddress getCurrentAddressToListenTo();
      static QHostAddress getMulticastGroup();
   };
}

#endif
