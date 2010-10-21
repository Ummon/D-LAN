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
      Common::Hash getOurID() const;
      void sendChatMessage(const QString& message);
      void setCoreSettings(const Protos::GUI::CoreSettings settings);

   signals:
      void coreConnected();
      void coreDisconnected();

      void newState(const Protos::GUI::State&);
      void newChatMessage(const Common::Hash& peerID, const QString& message);

   private slots:
      /*void connected();
      void disconnected();*/
      void dataReceived();

   private:
      bool readMessage();
      void send(quint32 type, const google::protobuf::Message& message);

      QTcpSocket socket;
      Common::Hash ourID;
      Common::MessageHeader currentHeader;
   };
}

#endif
