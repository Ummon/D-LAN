#include <QTest>
#include <QSignalSpy>
#include <QTcpServer>

#include <priv/InternalCoreConnection.h>

using Common::MessageHeader;

// Connect directly to the test listener, without starting a local core/service.
class TestConnection : public RCC::InternalCoreConnection
{
public:
   using InternalCoreConnection::InternalCoreConnection;
   void connectSocket(quint16 port) { this->socket->connectToHost(QHostAddress::LocalHost, port); }
};

class TestPeer : public Common::MessageSocket
{
   class Logger : public ILogger
   {
      void logDebug(const QString&) override {}
      void logError(const QString&) override {}
   };
public:
   TestPeer(QTcpSocket* socket) : MessageSocket(new Logger, socket, Common::Hash::rand()) { this->startListening(); }
};

class Tests : public QObject
{
   Q_OBJECT
   enum Kind { Chat, Browse, Search };
   RCC::CoreController controller;
   TestConnection connection {controller};
   QTcpServer server;
   QScopedPointer<TestPeer> peer;
   QList<int> results;
   QList<MessageHeader::MessageType> requests;
   Kind kind;

   void connectSession()
   {
      this->connection.connectSocket(this->server.serverPort());
      QTRY_VERIFY(this->server.hasPendingConnections());
      this->peer.reset(new TestPeer(this->server.nextPendingConnection()));
      connect(this->peer.data(), &Common::MessageSocket::newMessage, this, [this](const Common::Message& message) {
         if (message.getHeader().getType() != MessageHeader::GUI_LANGUAGE)
            this->requests << message.getHeader().getType();
      });
      Protos::GUI::AuthenticationResult auth;
      auth.set_status(Protos::GUI::AuthenticationResult::AUTH_OK);
      this->peer->send(MessageHeader::GUI_AUTHENTICATION_RESULT, auth);
      QTRY_VERIFY(this->connection.isConnected());
   }

   QSharedPointer<Common::Timeoutable> request(int id, int timeout = 5000)
   {
      auto record = [this, id] { this->results << id; };
      switch (this->kind)
      {
      case Chat:
      {
         auto result = this->connection.sendChatMessage(timeout, QString::number(id));
         connect(result.data(), &RCC::ISendChatMessageResult::result, this, record);
         return result;
      }
      case Browse:
      {
         auto result = this->connection.browse(Common::Hash::rand(), timeout);
         connect(result.data(), &RCC::IBrowseResult::result, this, record);
         return result;
      }
      case Search:
      {
         auto result = this->connection.search(Protos::Common::FindPattern(), false, timeout);
         connect(result.data(), &RCC::ISearchResult::result, this, record);
         return result;
      }
      }
      return {};
   }

   void start(const QSharedPointer<Common::Timeoutable>& request)
   {
      switch (this->kind)
      {
      case Chat: qobject_cast<RCC::ISendChatMessageResult*>(request.data())->start(); break;
      case Browse: qobject_cast<RCC::IBrowseResult*>(request.data())->start(); break;
      case Search: qobject_cast<RCC::ISearchResult*>(request.data())->start(); break;
      }
   }

   void tag(quint64 value)
   {
      if (this->kind == Chat)
         return;
      QSignalSpy received(&this->connection, &Common::MessageSocket::newMessage);
      Protos::GUI::Tag tag;
      tag.set_tag(value);
      this->peer->send(this->kind == Browse ? MessageHeader::GUI_BROWSE_TAG : MessageHeader::GUI_SEARCH_TAG, tag);
      QTRY_COMPARE(received.size(), 1);
   }

   void reply(quint64 value)
   {
      QSignalSpy received(&this->connection, &Common::MessageSocket::newMessage);
      switch (this->kind)
      {
      case Chat:
         this->peer->send(MessageHeader::GUI_CHAT_MESSAGE_RESULT, Protos::GUI::ChatMessageResult());
         break;
      case Browse:
      {
         Protos::GUI::BrowseResult result;
         result.set_tag(value);
         this->peer->send(MessageHeader::GUI_BROWSE_RESULT, result);
         break;
      }
      case Search:
      {
         Protos::Common::FindResult result;
         result.set_tag(value);
         this->peer->send(MessageHeader::GUI_SEARCH_RESULT, result);
         break;
      }
      }
      QTRY_COMPARE(received.size(), 1);
   }

private slots:
   void init()
   {
      QVERIFY(this->server.listen(QHostAddress::LocalHost));
      this->results.clear();
      this->requests.clear();
      this->connectSession();
   }

   void cleanup()
   {
      this->connection.disconnectFromCore();
      this->peer.reset();
      this->server.close();
   }

   void correlation_data()
   {
      QTest::addColumn<int>("requestKind");
      QTest::addColumn<QString>("scenario");
      for (int k : {Chat, Browse, Search})
         for (const auto& scenario : {"discarded", "unstarted", "reverse", "duplicate", "timeout", "reconnect", "tagged-reconnect"})
            QTest::newRow(qPrintable(QString::number(k) + "-" + scenario)) << k << QString(scenario);
   }

   void correlation()
   {
      QFETCH(int, requestKind);
      QFETCH(QString, scenario);
      this->kind = Kind(requestKind);
      auto first = this->request(1, scenario == "timeout" ? 10 : 5000);
      auto second = this->request(2);
      if (scenario == "unstarted")
      {
         this->start(second);
         QTRY_COMPARE(this->requests.size(), 1);
         this->tag(0);
         this->reply(0);
         QCOMPARE(this->results, QList<int>({2}));
         return;
      }
      if (scenario == "reverse")
      {
         this->start(second);
         this->start(first);
         QTRY_COMPARE(this->requests.size(), 2);
         this->tag(0);
         this->reply(0);
         this->tag(1);
         this->reply(1);
         QCOMPARE(this->results, QList<int>({2, 1}));
         return;
      }
      this->start(first);
      if (scenario == "reconnect" || scenario == "tagged-reconnect")
      {
         QTRY_COMPARE(this->requests.size(), 1);
         if (scenario == "tagged-reconnect")
            this->tag(0);
         // Keep both old request objects alive across a remote disconnect.
         this->peer->close();
         QTRY_VERIFY(!this->connection.isConnected());
         this->connectSession();
         this->requests.clear();
         this->start(second); // An unstarted request from the old session must not be sent.
         auto third = this->request(3);
         this->start(third);
         QTRY_COMPARE(this->requests.size(), 1);
         this->tag(0); // A new core may reuse tags from the previous session.
         this->reply(0);
         QCOMPARE(this->results, QList<int>({3}));
         return;
      }
      if (scenario == "duplicate")
         this->start(first);
      if (scenario == "timeout")
         QTRY_VERIFY(first->isTimedout());
      this->start(second);
      QTRY_COMPARE(this->requests.size(), 2);
      if (scenario == "discarded" || scenario == "timeout")
         first.clear();
      this->tag(0);
      this->reply(0);
      QCOMPARE(this->results, scenario == "duplicate" ? QList<int>({1}) : QList<int>());
      this->tag(1);
      this->reply(1);
      QCOMPARE(this->results, scenario == "duplicate" ? QList<int>({1, 2}) : QList<int>({2}));
   }
};

QTEST_GUILESS_MAIN(Tests)
#include "Tests.moc"
