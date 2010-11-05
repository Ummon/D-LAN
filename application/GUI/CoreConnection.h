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

   class IBrowseResult : public QObject
   {
      Q_OBJECT
   public:
      virtual ~IBrowseResult() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::Common::Entries& entries);
      // void timeout(); TODO
   };

   class ISearchResult : public QObject
   {
      Q_OBJECT
   public:
      virtual ~ISearchResult() {}
      virtual void start() = 0;

   signals:
      void result(const Protos::Common::FindResult&);
      // void timeout(); TODO
   };

   class BrowseResult : public IBrowseResult
   {
      Q_OBJECT
   public:
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID);
      BrowseResult(CoreConnection* coreConnection, const Common::Hash& peerID, const Protos::Common::Entry& entry);
      void start();
      void setTag(quint64 tag);

   private slots:
      void browseResult(quint64 tag, const Protos::Common::Entries& entries);

   private:
      void init(CoreConnection* coreConnection);

      CoreConnection* coreConnection;
      const Common::Hash peerID;
      const Protos::Common::Entry entry; // Not sure if it's save to not copy the entry...
      quint64 tag;
   };

   class SearchResult : public ISearchResult
   {
      Q_OBJECT
   public:
      SearchResult(CoreConnection* coreConnection, const QString& terms);
      void start();
      void setTag(quint64 tag);

   private slots:
      void searchResult(const Protos::Common::FindResult& findResult);

   private:
      CoreConnection* coreConnection;
      const QString terms;
      quint64 tag;
   };

   class CoreConnection : public QObject
   {
      Q_OBJECT
   public:
      CoreConnection();
      Common::Hash getOurID() const;
      void sendChatMessage(const QString& message);
      void setCoreSettings(const Protos::GUI::CoreSettings settings);

      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID);
      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entry& entry);

      QSharedPointer<ISearchResult> search(const QString& terms);

      void download(const Common::Hash& peerID, const Protos::Common::Entry& entry);

   public slots:
      void connectToCore();

   signals:
      void coreConnected();
      void coreDisconnected();

      void newState(const Protos::GUI::State&);
      void newChatMessage(const Common::Hash& peerID, const QString& message);
      void browseResult(quint64 tag, const Protos::Common::Entries& entries);
      void searchResult(const Protos::Common::FindResult& findResult);

   private slots:
      void stateChanged(QAbstractSocket::SocketState socketState);
      void dataReceived();

   private:
      friend class BrowseResult;
      friend class SearchResult;

      void send(Common::Network::GUIMessageType type, const google::protobuf::Message& message);
      bool readMessage();

      QTcpSocket socket;
      Common::Hash ourID;
      Common::Network::MessageHeader<Common::Network::GUIMessageType> currentHeader;

      QList< QSharedPointer<BrowseResult> > browseResultsWithoutTag;
      QList< QSharedPointer<SearchResult> > searchResultsWithoutTag;
      bool connecting;
   };
}

#endif
