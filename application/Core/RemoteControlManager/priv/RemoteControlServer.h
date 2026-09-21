#pragma once

#include <QTcpServer>
#include <QSslSocket>

namespace RCM
{
   // Adopt descriptors directly: wrapping an already active QTcpSocket would
   // leave two socket objects owning the same native descriptor.
   class RemoteControlServer : public QTcpServer
   {
   protected:
      void incomingConnection(qintptr descriptor) override
      {
         auto* socket = new QSslSocket(this);
         if (socket->setSocketDescriptor(descriptor))
            this->addPendingConnection(socket);
         else
            delete socket;
      }
   };
}
