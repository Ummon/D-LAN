#ifndef GUI_CORECONNECTION_H
#define GUI_CORECONNECTION_H

#include <QObject>
#include <QTcpSocket>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/Network.h>

namespace GUI
{
   class CoreConnection : public QObject
   {
      Q_OBJECT
   public:
      CoreConnection();
      void connectToCore();

   signals:
      void coreConnected();
      void coreDisconnected();

      void newState(const Protos::GUI::State&);

   private slots:
      /*void connected();
      void disconnected();*/
      void dataReceived();

   private:
      bool readMessage();

      QTcpSocket socket;
      Common::MessageHeader currentHeader;
   };
}

#endif
