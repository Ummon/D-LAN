#pragma once

#include <QTcpSocket>
#include <QTimer>

#include <Common/Network/MessageSocket.h>

#include <State.h>

namespace DummyCore
{
   /**
     * A connected GUI. It's the counterpart of 'RCM::RemoteConnection' in the Core: the protocol is the one
     * of "application/Protos/gui_protocol.proto" but the answers only depend on the static state.
     */
   class Connection : public Common::MessageSocket
   {
      Q_OBJECT

      static const int REFRESH_RATE = 1000; // [ms]. As the default value of the setting 'remote_refresh_rate' of the Core.

      class Logger : public ILogger
      {
      public:
         void logDebug(const QString& message) override;
         void logError(const QString& message) override;
      };

   public:
      /**
        * Takes ownership of 'socket'.
        */
      Connection(const State& state, QTcpSocket* socket);

   private:
      void refresh();

      void onStartListening() override;
      void onNewMessage(const Common::Message& message) override;
      void onDisconnected() override;

      const State& state;

      bool waitForStateResult = false; // The GUI acknowledges each state, the next one isn't sent before.
      QTimer timerRefresh;
   };
}
