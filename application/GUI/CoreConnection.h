#ifndef GUI_CORECONNECTION_H
#define GUI_CORECONNECTION_H

#include <QObject>
#include <QTcpSocket>
#include <QSharedPointer>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/Network.h>

namespace GUI
{
   class CoreConnection;

   class BrowseResult : public QObject
   {
      Q_OBJECT
   public:
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID);
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entry& entry);
      void start();

      void setTag(quint64 tag);

   signals:
      void result(const Protos::Common::Entries& entries);
      // void timeout(); TODO

   private slots:
      void browseResult(quint64 tag, const Protos::Common::Entries& entries);

   private:
      void init(CoreConnection* coreConnection);

      CoreConnection* coreConnection;
      Common::Hash peerID;
      const Protos::Common::Entry entry; // Not sure if it's save to not copy the entry...
      quint64 tag;
   };

   class CoreConnection : public QObject
   {
      Q_OBJECT
   public:
      CoreConnection();
      void connectToCore();
      Common::Hash getOurID() const;
      void sendChatMessage(const QString& message);
      void setCoreSettings(const Protos::GUI::CoreSettings settings);

      QSharedPointer<BrowseResult> browse(const Common::Hash& peerID);
      QSharedPointer<BrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entry& entry);

      void send(quint32 type, const google::protobuf::Message& message);

   signals:
      void coreConnected();
      void coreDisconnected();

      void newState(const Protos::GUI::State&);
      void newChatMessage(const Common::Hash& peerID, const QString& message);
      void browseResult(quint64 tag, const Protos::Common::Entries& entries);

   private slots:
      /*void connected();
      void disconnected();*/
      void dataReceived();

   private:
      bool readMessage();

      QTcpSocket socket;
      Common::Hash ourID;
      Common::MessageHeader currentHeader;

      QList< QSharedPointer<BrowseResult> > browseResultsWithoutTag;
   };
}

#endif
