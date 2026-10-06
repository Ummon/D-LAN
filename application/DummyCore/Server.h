#pragma once

#include <QObject>
#include <QString>
#include <QTcpServer>

#include <State.h>

namespace DummyCore
{
   /**
     * Accepts the GUIs, as 'RCM::RemoteControlManager' does in the Core.
     */
   class Server : public QObject
   {
      Q_OBJECT
   public:
      /**
        * 'state' must outlive the server.
        */
      explicit Server(const State& state);

      /**
        * Only the connections from this computer are accepted: nothing is asked to the GUI to authenticate itself.
        * @param port 0 to let the system choose a port, see 'getPort()'.
        * @return An error message, empty if OK.
        */
      QString listen(quint16 port);

      quint16 getPort() const;

   private:
      void newConnection(QTcpServer& server);

      const State& state;

      QTcpServer serverIPv4;
      QTcpServer serverIPv6;
   };
}
