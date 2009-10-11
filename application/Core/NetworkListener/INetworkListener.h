#ifndef NETWORKMANAGER_INETWORKLISTENER_H
#define NETWORKMANAGER_INETWORKLISTENER_H

#include <QObject>

namespace NetworkListener
{
   class IChat;
   class ISearch;

   class INetworkListener : public QObject {
       Q_OBJECT

       public:
          virtual ~INetworkListener() {}

          virtual IChat* getChat() = 0;
          //virtual ISearch* search() = 0;

       /* Dues to a limiation of QObject (cannot inherit more than one QObject class), we must have the def of
        presence, used in NetworkListener, here. TODO: Find a better solution ? */
        public slots:
            virtual void presence() = 0;
   };
}
#endif
