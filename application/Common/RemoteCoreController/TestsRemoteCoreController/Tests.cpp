#include <QTest>
#include <QSignalSpy>
#include <QTcpServer>
#include <QScopeGuard>
#include <memory>

#include <priv/InternalCoreConnection.h>
#include <priv/CoreConnection.h>

using Common::MessageHeader;

// Connect directly to the test listener, without starting a local core/service.
class TestConnection : public RCC::InternalCoreConnection
{
public:
   using InternalCoreConnection::InternalCoreConnection;
   void connectSocket(quint16 port) { this->socket->connectToHost(QHostAddress::LocalHost, port); }
   bool deferNextClose = false;
   void close() override
   {
      if (this->deferNextClose)
      {
         this->deferNextClose = false;
         QTimer::singleShot(0, this, [this] { this->MessageSocket::close(); });
      }
      else
         this->MessageSocket::close();
   }
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
   QList<Protos::GUI::Browse> browseRequests;
   QList<Protos::GUI::Search> searchRequests;
   Kind kind;

   void connectSession()
   {
      this->connection.connectSocket(this->server.serverPort());
      QTRY_VERIFY(this->server.hasPendingConnections());
      this->peer.reset(new TestPeer(this->server.nextPendingConnection()));
      connect(this->peer.data(), &Common::MessageSocket::newMessage, this, [this](const Common::Message& message) {
         if (message.getHeader().getType() != MessageHeader::GUI_LANGUAGE)
            this->requests << message.getHeader().getType();
         if (message.getHeader().getType() == MessageHeader::GUI_BROWSE)
            this->browseRequests << message.getMessage<Protos::GUI::Browse>();
         if (message.getHeader().getType() == MessageHeader::GUI_SEARCH)
            this->searchRequests << message.getMessage<Protos::GUI::Search>();
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

   void replyToRequest(int index)
   {
      if (this->kind == Browse)
      {
         QVERIFY(index < this->browseRequests.size());
         this->reply(this->browseRequests[index].tag());
      }
      else if (this->kind == Search)
      {
         QVERIFY(index < this->searchRequests.size());
         this->reply(this->searchRequests[index].tag());
      }
      else
         this->reply(index);
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
   void delayedCoreStartup_data()
   {
      QTest::addColumn<QString>("outcome");
      QTest::addColumn<bool>("multipleAddresses");
      QTest::newRow("listener-starts-later") << QString("connected") << false;
      QTest::newRow("ipv6-to-ipv4-fallback") << QString("connected") << true;
      QTest::newRow("listener-never-starts") << QString("timeout") << false;
      QTest::newRow("disconnect-during-authentication") << QString("disconnect") << false;
   }

   void delayedCoreStartup()
   {
      QFETCH(QString, outcome);
      QFETCH(bool, multipleAddresses);
#ifdef Q_OS_UNIX
      RCC::CoreConnection core;
      auto& process = core.coreController.coreProcess;
      process.start("/bin/cat", QStringList());
      QVERIFY(process.waitForStarted());
      const auto cleanup = qScopeGuard([&] { process.kill(); process.waitForFinished(); });

      QTcpServer delayedServer;
      QVERIFY(delayedServer.listen(QHostAddress::LocalHost));
      const quint16 port = delayedServer.serverPort();
      delayedServer.close();
      QSignalSpy errors(&core, &RCC::ICoreConnection::connectingError);
      QSignalSpy connected(&core, &RCC::ICoreConnection::connected);
      auto& pending = core.temp();
      QVERIFY(core.connectToCorePrepare("127.0.0.1"));
      pending.connectionInfo = {"127.0.0.1", port, Common::Hash()};
      if (multipleAddresses)
         pending.addressesToTry << QHostAddress::LocalHostIPv6;
      pending.addressesToTry << QHostAddress::LocalHost;
      pending.tryToConnectToTheNextAddress();
      QTRY_VERIFY(pending.retryTimer.isActive());
      QVERIFY(core.isConnecting());
      QCOMPARE(errors.count(), 0);
      // A refused TCP connection must not become a public timeout, regardless
      // of whether the platform emits disconnected for that failed attempt.
      pending.onDisconnected();
      QCOMPARE(errors.count(), 0);
      QVERIFY(core.isConnecting());

      if (outcome == "timeout")
      {
         QTRY_COMPARE_WITH_TIMEOUT(errors.count(), 1, 5000);
         QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::RCC_ERROR_HOST_TIMEOUT);
         QVERIFY(!core.isConnecting());
         QVERIFY(!pending.retryTimer.isActive());
         QVERIFY(!pending.connectionTimeoutTimer.isActive());
         QTest::qWait(300);
         QCOMPARE(errors.count(), 1);
         QCOMPARE(connected.count(), 0);
         return;
      }

      QTest::qWait(550); // More than one refused connection while the core starts.
      QCOMPARE(errors.count(), 0);
      QVERIFY(core.isConnecting());
      QVERIFY(delayedServer.listen(QHostAddress::LocalHost, port));
      QTRY_VERIFY(delayedServer.hasPendingConnections());
      auto* remote = delayedServer.nextPendingConnection();
      TestPeer delayedPeer(remote);
      if (outcome == "disconnect")
      {
         QTRY_COMPARE(pending.socket->state(), QAbstractSocket::ConnectedState);
         remote->abort();
         QTRY_COMPARE(errors.count(), 1);
         QVERIFY(!core.isConnecting());
         QVERIFY(!pending.retryTimer.isActive());
         QCOMPARE(connected.count(), 0);
         return;
      }
      Protos::GUI::AuthenticationResult auth;
      auth.set_status(Protos::GUI::AuthenticationResult::AUTH_OK);
      delayedPeer.send(MessageHeader::GUI_AUTHENTICATION_RESULT, auth);
      QTRY_COMPARE(connected.count(), 1);
      QCOMPARE(errors.count(), 0);
      QVERIFY(core.isConnected());
      QVERIFY(!pending.connectionTimeoutTimer.isActive());
      core.disconnectFromCore();
#else
      QSKIP("Uses a Unix subprocess fixture");
#endif
   }

   void subprocessDestructionDoesNotEmitStatus()
   {
#ifdef Q_OS_UNIX
      auto connection = std::make_unique<RCC::CoreConnection>(1000);
      auto& process = connection->coreController.coreProcess;
      // Keep a harmless subprocess alive without touching the user's core/service.
      process.start("/bin/cat", QStringList());
      QVERIFY(process.waitForStarted());
      QSignalSpy status(connection.get(), &RCC::CoreConnection::localCoreStatusChanged);
      connection.reset();
      // A status callback here could query members already destroyed by teardown.
      QCOMPARE(status.count(), 0);
#else
      QSKIP("Uses a Unix subprocess fixture");
#endif
   }

   void init()
   {
      QVERIFY(this->server.listen(QHostAddress::LocalHost));
      this->results.clear();
      this->requests.clear();
      this->browseRequests.clear();
      this->searchRequests.clear();
      this->connectSession();
   }

   void cleanup()
   {
      this->connection.disconnectFromCore();
      this->peer.reset();
      this->server.close();
   }

   void cancellation_data()
   {
      QTest::addColumn<QString>("stage");
      for (const auto& stage : {"lookup", "retry", "socket", "authentication"})
         QTest::newRow(stage) << QString(stage);
   }

   void cancellation()
   {
      QFETCH(QString, stage);
      RCC::CoreConnection core;
      QSignalSpy connected(&core, &RCC::ICoreConnection::connected);
      QSignalSpy errors(&core, &RCC::ICoreConnection::connectingError);
      QSignalSpy disconnected(&core, &RCC::ICoreConnection::disconnected);
      auto& pending = core.temp();
      QScopedPointer<TestPeer> pendingPeer;
      int cancelledLookup = -1;
      if (stage == "lookup")
      {
         // Cancel before processing events, while even a cached lookup is pending.
         core.connectToCore("localhost", this->server.serverPort(), Common::Hash());
         cancelledLookup = pending.currentHostLookupID;
         QVERIFY(cancelledLookup != -1);
      }
      else
      {
         QVERIFY(core.connectToCorePrepare("localhost"));
         pending.connectionInfo = {"localhost", this->server.serverPort(), Common::Hash()};
         if (stage == "retry")
         {
            // Enter the retry delay deterministically, without a real connection timeout.
            pending.addressesToRetry << QHostAddress("192.0.2.1");
            pending.stateChanged(QAbstractSocket::UnconnectedState);
            QVERIFY(pending.retryTimer.isActive());
         }
         else
         {
            pending.socket->connectToHost(QHostAddress::LocalHost, this->server.serverPort());
            if (stage == "authentication")
            {
               QTRY_VERIFY(this->server.hasPendingConnections());
               pendingPeer.reset(new TestPeer(this->server.nextPendingConnection()));
               QTRY_COMPARE(pending.socket->state(), QAbstractSocket::ConnectedState);
               QVERIFY(!pending.isConnected()); // Authentication has not arrived.
            }
            else
            {
               QCOMPARE(pending.socket->state(), QAbstractSocket::ConnectingState);
               pending.addressesToTry << QHostAddress("192.0.2.1");
               connect(pending.socket, &QAbstractSocket::stateChanged, &pending, &RCC::InternalCoreConnection::stateChanged);
            }
         }
      }
      QVERIFY(core.isConnecting());
      core.disconnectFromCore();
      QVERIFY(!core.isConnecting());
      QVERIFY(!core.isConnected());
      QCOMPARE(pending.currentHostLookupID, -1);
      QVERIFY(!pending.retryTimer.isActive());
      QVERIFY(pending.addressesToTry.isEmpty());
      QVERIFY(pending.addressesToRetry.isEmpty());
      QCOMPARE(pending.socket->state(), QAbstractSocket::UnconnectedState);
      QVERIFY(pending.connectionInfo.address.isEmpty());
      QCOMPARE(pending.connectionInfo.port, 0);
      core.disconnectFromCore(); // Repeated cancellation is harmless.

      if (stage == "lookup")
      {
         // A late result must not revive the cancelled attempt or abort its replacement.
         QHostInfo stale(cancelledLookup);
         stale.setAddresses({QHostAddress("192.0.2.1")});
         pending.addressResolved(stale);
         QCOMPARE(pending.socket->state(), QAbstractSocket::UnconnectedState);
         core.connectToCore("localhost", this->server.serverPort(), Common::Hash());
         const int replacementLookup = pending.currentHostLookupID;
         QVERIFY(replacementLookup != -1);
         QVERIFY(replacementLookup != cancelledLookup);
         pending.addressResolved(stale);
         QCOMPARE(pending.currentHostLookupID, replacementLookup);
         QVERIFY(core.isConnecting());
         core.disconnectFromCore();
      }

      QTest::qWait(350); // Longer than the retry delay on both Windows and Linux.
      QCOMPARE(pending.socket->state(), QAbstractSocket::UnconnectedState);
      QCOMPARE(connected.size(), 0);
      QCOMPARE(errors.size(), 0);
      QCOMPARE(disconnected.size(), 0); // No established session was disconnected.
      // A connection cancelled in ConnectingState may still have reached the listener.
      while (this->server.hasPendingConnections())
         delete this->server.nextPendingConnection();
      pendingPeer.reset();

      // A fresh attempt must still reach the public connected/disconnected signals.
      QVERIFY(core.connectToCorePrepare("localhost"));
      pending.socket->connectToHost(QHostAddress::LocalHost, this->server.serverPort());
      QTRY_VERIFY(this->server.hasPendingConnections());
      pendingPeer.reset(new TestPeer(this->server.nextPendingConnection()));
      Protos::GUI::AuthenticationResult auth;
      auth.set_status(Protos::GUI::AuthenticationResult::AUTH_OK);
      pendingPeer->send(MessageHeader::GUI_AUTHENTICATION_RESULT, auth);
      QTRY_COMPARE(connected.size(), 1);
      QVERIFY(core.isConnected());
      QVERIFY(!core.isConnecting());
      QCOMPARE(errors.size(), 0);
      core.disconnectFromCore();
      QTRY_COMPARE(disconnected.size(), 1);
      QCOMPARE(disconnected[0][0].toBool(), true);
      QVERIFY(!core.isConnected());
   }

   void deferredDisconnection()
   {
      QSignalSpy disconnected(&this->connection, &RCC::InternalCoreConnection::disconnected);
      // Make shutdown asynchronous regardless of how quickly localhost drains writes.
      this->connection.deferNextClose = true;
      this->connection.disconnectFromCore();
      QCOMPARE(disconnected.size(), 0);
      QTRY_COMPARE(disconnected.size(), 1);
      QCOMPARE(disconnected[0][0].toBool(), true);
      QVERIFY(!this->connection.isConnected());

      // Cancelling an idle connection must not mark a later remote close as requested.
      this->connection.disconnectFromCore();
      QCOMPARE(disconnected.size(), 1);
      this->connectSession();
      this->peer->close();
      QTRY_COMPARE(disconnected.size(), 2);
      QCOMPARE(disconnected[1][0].toBool(), false);
   }

   void browseReplies()
   {
      this->kind = Browse;
      const auto peerID = Common::Hash::rand();
      Protos::Common::Entry entry;
      entry.set_name("directory");
      Protos::Common::Entries entries;
      entries.add_entries()->CopyFrom(entry);
      const QList<QSharedPointer<RCC::IBrowseResult>> pending {
         this->connection.browse(peerID, 5000),
         this->connection.browse(peerID, entry, 5000),
         this->connection.browse(peerID, entries, true, 5000)
      };
      for (int i = 0; i < pending.size(); ++i)
      {
         connect(pending[i].data(), &RCC::IBrowseResult::result, this, [this, i] { this->results << i; });
         pending[i]->start();
      }
      QTRY_COMPARE(this->browseRequests.size(), 3);
      QSet<quint64> tags;
      for (const auto& request : this->browseRequests)
      {
         QCOMPARE(Common::Hash(request.peer_id().hash()), peerID);
         tags.insert(request.tag());
      }
      QCOMPARE(tags.size(), 3);
      QCOMPARE(this->browseRequests[0].dirs().entries_size(), 0);
      QCOMPARE(this->browseRequests[1].dirs().entries(0).name(), entry.name());
      QCOMPARE(this->browseRequests[2].dirs().entries(0).name(), entry.name());
      QVERIFY(this->browseRequests[2].get_roots());
      quint64 unknownTag = 0;
      while (tags.contains(unknownTag))
         ++unknownTag;
      this->reply(unknownTag);
      QVERIFY(this->results.isEmpty());
      for (int i : {2, 0, 1})
      {
         this->replyToRequest(i);
         this->replyToRequest(i); // Repeated replies must only emit one result.
      }
      QCOMPARE(this->results, QList<int>({2, 0, 1}));
   }

   void searchReplies()
   {
      this->kind = Search;
      Protos::Common::FindPattern pattern;
      pattern.set_pattern("test files");
      QList<QSharedPointer<RCC::ISearchResult>> pending;
      for (int i = 0; i < 2; ++i)
      {
         auto search = this->connection.search(pattern, i == 0, 5000);
         connect(search.data(), &RCC::ISearchResult::result, this, [this, i] { this->results << i; });
         pending << search;
         search->start();
      }
      QTRY_COMPARE(this->searchRequests.size(), 2);
      QVERIFY(this->searchRequests[0].tag() != this->searchRequests[1].tag());
      for (int i = 0; i < 2; ++i)
      {
         QCOMPARE(this->searchRequests[i].pattern().pattern(), pattern.pattern());
         QCOMPARE(this->searchRequests[i].local(), i == 0);
      }
      quint64 unknownTag = 0;
      while (unknownTag == this->searchRequests[0].tag() || unknownTag == this->searchRequests[1].tag())
         ++unknownTag;
      this->reply(unknownTag);
      QVERIFY(this->results.isEmpty());
      // Searches keep receiving batches, including interleaved and out-of-order replies.
      for (int i : {1, 0, 1, 0})
         this->replyToRequest(i);
      QCOMPARE(this->results, QList<int>({1, 0, 1, 0}));
   }

   void lateReplyAfterTimeout_data()
   {
      QTest::addColumn<int>("requestKind");
      QTest::newRow("browse") << int(Browse);
      QTest::newRow("search") << int(Search);
   }

   void lateReplyAfterTimeout()
   {
      QFETCH(int, requestKind);
      this->kind = Kind(requestKind);
      auto pending = this->request(1, 10);
      this->start(pending);
      QTRY_COMPARE(this->requests.size(), 1);
      QTRY_VERIFY(pending->isTimedout());
      this->replyToRequest(0);
      QVERIFY(this->results.isEmpty());
   }

   void correlation_data()
   {
      QTest::addColumn<int>("requestKind");
      QTest::addColumn<QString>("scenario");
      for (int k : {Chat, Browse, Search})
         for (const auto& scenario : {"discarded", "unstarted", "reverse", "duplicate", "timeout", "reconnect"})
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
         this->replyToRequest(0);
         QCOMPARE(this->results, QList<int>({2}));
         return;
      }
      if (scenario == "reverse")
      {
         this->start(second);
         this->start(first);
         QTRY_COMPARE(this->requests.size(), 2);
         this->replyToRequest(0);
         this->replyToRequest(1);
         QCOMPARE(this->results, QList<int>({2, 1}));
         return;
      }
      this->start(first);
      if (scenario == "reconnect")
      {
         QTRY_COMPARE(this->requests.size(), 1);
         const quint64 oldTag = this->kind == Browse ? this->browseRequests[0].tag() :
            this->kind == Search ? this->searchRequests[0].tag() : 0;
         // Keep both old request objects alive across a remote disconnect.
         this->peer->close();
         QTRY_VERIFY(!this->connection.isConnected());
         this->connectSession();
         this->requests.clear();
         this->browseRequests.clear();
         this->searchRequests.clear();
         this->start(second); // An unstarted request from the old session must not be sent.
         auto third = this->request(3);
         this->start(third);
         QTRY_COMPARE(this->requests.size(), 1);
         if (this->kind != Chat)
         {
            this->reply(oldTag); // A late reply must not revive the old request.
            QVERIFY(this->results.isEmpty());
         }
         this->replyToRequest(0);
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
      this->replyToRequest(0);
      QCOMPARE(this->results, scenario == "duplicate" ? QList<int>({1}) : QList<int>());
      this->replyToRequest(1);
      QCOMPARE(this->results, scenario == "duplicate" ? QList<int>({1, 2}) : QList<int>({2}));
   }
};

QTEST_GUILESS_MAIN(Tests)
#include "Tests.moc"
