#include <limits>
#include <stdexcept>

#include <QTest>
#include <QPointer>
#include <QSignalSpy>
#include <QTemporaryDir>
#include <QSemaphore>
#include <QScopeGuard>
#include <priv/LocalBrowse.h>

#include <Common/Settings.h>
#include <Common/Global.h>
#include <Common/ProtoHelper.h>
#include <Core/PeerManager/Builder.h>
#include <Protos/core_settings.pb.h>
#include "Mocks.h"

#include <Core/PeerManager/GetChunkParams.h>
#include <priv/UploadProgress.h>

class Tests : public QObject
{
   Q_OBJECT

private slots:
   void initTestCase()
   {
      QVERIFY(this->dataDirectory.isValid());
      Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, this->dataDirectory.path());
      Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, this->dataDirectory.path());
      auto settings = new Protos::Core::Settings;
      settings->set_peer_timeout_factor(3);
      settings->set_peer_imalive_period(5000);
      settings->set_remote_refresh_rate(1000);
      settings->set_delay_before_sending_log_messages(100);
      settings->set_delay_gui_connection_fail(10);
      SETTINGS.setSettingsMessage(settings);
      SETTINGS.set("peer_id", Common::Hash::rand());
      SETTINGS.set("remote_password", Common::Hash::rand());
      SETTINGS.set("salt", quint64(123));
   }

   void bufferedLocalAuthentication()
   {
      auto socket = new BufferedSocket;
      socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, Protos::GUI::Authentication());
      Protos::GUI::Language language;
      Common::ProtoHelper::setLang(*language.mutable_language(), QLocale("fr_CH"));
      socket->receive(Common::MessageHeader::GUI_LANGUAGE, language);

      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      QSignalSpy languageDefined(connection, &RCM::RemoteConnection::languageDefined);
      QVERIFY(socket->output.isEmpty());
      QVERIFY(!socket->input.isEmpty());
      connection->startListening();

      auto messages = socket->messages();
      QCOMPARE(messages.size(), 4); // Challenge, authentication result, state, chat history.
      QCOMPARE(messages[0].getHeader().getType(), Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION);
      QCOMPARE(messages[1].getMessage<Protos::GUI::AuthenticationResult>().status(), Protos::GUI::AuthenticationResult::AUTH_OK);
      QCOMPARE(languageDefined.size(), 1);
      QCOMPARE(languageDefined[0][0].value<QLocale>(), QLocale("fr_CH"));

      connection->startListening();
      QCOMPARE(socket->messages().size(), 4);
      QTest::qWait(5500); // A successful buffered response must cancel the original deadline.
      QVERIFY(connection);
      QVERIFY(connection->isConnected());
      delete connection;
   }

   void remoteAuthentication_data()
   {
      QTest::addColumn<bool>("early");
      QTest::newRow("buffered-zero-challenge") << true;
      QTest::newRow("normal-challenge-response") << false;
   }

   void remoteAuthentication()
   {
      QFETCH(bool, early);
      auto socket = new BufferedSocket(false);
      const auto password = SETTINGS.get<Common::Hash>("remote_password");
      auto authenticate = [&](quint64 challenge) {
         Protos::GUI::Authentication authentication;
         const auto hash = Common::Hasher::hashWithSalt(password, challenge);
         authentication.mutable_password_challenge()->set_hash(hash.getData(), Common::Hash::HASH_SIZE);
         socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, authentication);
      };
      if (early)
         authenticate(0);
      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      QList<Common::Message> finalMessages;
      connect(connection, &RCM::RemoteConnection::deleted, this, [&] { finalMessages = socket->messages(); });
      connection->startListening();
      const auto messages = socket->messages();
      QCOMPARE(messages.size(), 1);
      QCOMPARE(messages[0].getHeader().getType(), Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION);
      const auto challenge = messages[0].getMessage<Protos::GUI::AskForAuthentication>();
      QCOMPARE(challenge.salt(), quint64(123));
      if (early)
      {
         QTRY_VERIFY(!connection);
         QCOMPARE(finalMessages.size(), 2);
         QCOMPARE(finalMessages[1].getMessage<Protos::GUI::AuthenticationResult>().status(), Protos::GUI::AuthenticationResult::AUTH_BAD_PASSWORD);
      }
      else
      {
         authenticate(challenge.salt_challenge());
         QCOMPARE(socket->messages().size(), 4);
         QCOMPARE(socket->messages()[1].getMessage<Protos::GUI::AuthenticationResult>().status(), Protos::GUI::AuthenticationResult::AUTH_OK);
         delete connection;
      }
   }

   void disconnectedDuringStartup_data()
   {
      QTest::addColumn<bool>("malformed");
      QTest::newRow("already-closed") << false;
      QTest::newRow("buffered-invalid-header") << true;
   }

   void disconnectedDuringStartup()
   {
      QFETCH(bool, malformed);
      auto socket = new BufferedSocket;
      if (malformed)
      {
         socket->input.resize(Common::MessageHeader::HEADER_SIZE);
         Common::MessageHeader::writeHeader(socket->input.data(),
            Common::MessageHeader(Common::MessageHeader::GUI_AUTHENTICATION, std::numeric_limits<quint32>::max(), Common::Hash()));
      }
      else
         socket->close();
      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      QSignalSpy deleted(connection, &RCM::RemoteConnection::deleted);
      connection->startListening();
      connection->startListening();
      QTRY_VERIFY(!connection);
      QCOMPARE(deleted.size(), 1);
   }

   void uploadProgress_data()
   {
      QTest::addColumn<quint64>("size");
      QTest::addColumn<quint64>("owned");
      QTest::addColumn<int>("offset");
      QTest::addColumn<int>("expected");

      const quint64 maximum = std::numeric_limits<quint64>::max();
      const quint64 signedMaximum = std::numeric_limits<qint64>::max();
      QTest::newRow("empty") << quint64(0) << maximum << 1 << 0;
      QTest::newRow("not-started") << quint64(100) << quint64(0) << 0 << 0;
      QTest::newRow("partial") << quint64(100) << quint64(20) << 5 << 2500;
      QTest::newRow("fraction") << quint64(3) << quint64(1) << 0 << 3333;
      QTest::newRow("complete") << quint64(100) << quint64(90) << 10 << 10000;
      QTest::newRow("offset-exceeds-remaining") << quint64(100) << quint64(90) << 20 << 10000;
      QTest::newRow("negative-offset") << quint64(100) << quint64(25) << -1 << 2500;
      QTest::newRow("untrusted-maximum") << quint64(100) << maximum << 1 << 10000;
      QTest::newRow("signed-addition-overflow") << signedMaximum << signedMaximum << 1 << 10000;
      QTest::newRow("multiplication-overflow") << (quint64(1) << 62) << (quint64(1) << 61) << 0 << 5000;
      QTest::newRow("unsigned-addition-overflow") << maximum << (maximum - 1) << 2 << 10000;
      QTest::newRow("maximum-incomplete") << maximum << (maximum - 1) << 0 << 9999;
   }

   void localBrowseContents()
   {
      QTemporaryDir directory;
      QVERIFY(directory.isValid());
      QDir root(directory.path());
      QVERIFY(root.mkdir("empty"));
      QVERIFY(root.mkdir("nonempty"));
      for (const auto& name : {"file.txt", "nonempty/child.txt"})
      {
         QFile file(root.filePath(name));
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write("hello"), qint64(5));
      }
      auto socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket));
      connection->startListening();
      socket->output.clear();
      Protos::GUI::LocalBrowse request;
      request.set_path(directory.path().toStdString());
      request.set_tag(1234);
      socket->receive(Common::MessageHeader::GUI_LOCAL_BROWSE, request);
      QTRY_COMPARE(socket->messages().size(), 1);
      const auto message = socket->messages()[0];
      QCOMPARE(message.getHeader().getType(), Common::MessageHeader::GUI_LOCAL_BROWSE_RESULT);
      const auto& result = message.getMessage<Protos::GUI::LocalBrowseResult>();
      QCOMPARE(result.tag(), quint64(1234));
      QCOMPARE(result.entries_size(), 3);
      QMap<QString, qint64> sizes;
      for (const auto& entry : result.entries())
      {
         sizes.insert(QString::fromStdString(entry.name()), entry.size());
         QCOMPARE(entry.type(), entry.name() == "file.txt" ? Protos::GUI::LocalBrowseResult::FILE : Protos::GUI::LocalBrowseResult::DIR);
         QVERIFY(entry.date_modified() > 0);
      }
      QCOMPARE(sizes.value("empty", -1), qint64(0));
      QCOMPARE(sizes.value("nonempty", -1), qint64(1));
      QCOMPARE(sizes.value("file.txt", -1), qint64(5));
   }

   void localBrowseDoesNotBlockConnection_data()
   {
      QTest::addColumn<bool>("overload");
      QTest::newRow("disconnect-with-pending-work") << false;
      QTest::newRow("bounded-pending-work") << true;
   }

   void localBrowseQueueIsBoundedAndCancellationFreesSlots()
   {
      QSemaphore started;
      QSemaphore release;
      auto& pool = RCM::localBrowsePool();
      for (int i = 0; i < 2; ++i)
         pool.start([&] { started.release(); release.acquire(); });
      const auto unblock = qScopeGuard([&] { release.release(2); pool.waitForDone(); });
      QVERIFY(started.tryAcquire(2, 3000));

      Protos::GUI::LocalBrowse request;
      request.set_path(this->dataDirectory.path().toStdString());
      QList<RCM::LocalBrowseJob> jobs;
      const auto cancelJobs = qScopeGuard([&] { for (const auto& job : jobs) job.cancel(); });
      for (int i = 0; i < 40; ++i)
      {
         jobs << RCM::localBrowse(request);
         QVERIFY(!jobs.last().future.isFinished());
      }
      auto rejected = RCM::localBrowse(request);
      QVERIFY(rejected.future.isFinished());
      QVERIFY_THROWS_EXCEPTION(std::runtime_error, rejected.future.result());

      for (const auto& job : jobs)
      {
         job.cancel();
         QVERIFY(job.future.isCanceled());
         QVERIFY(job.future.isFinished()); // No worker had to run the cancelled job.
         job.cancel(); // Cancellation is idempotent.
      }
      jobs.clear();

      // Reconnecting must not leave cancelled requests behind the blocked workers.
      for (int cycle = 0; cycle < 12; ++cycle)
      {
         auto* socket = new BufferedSocket;
         auto* connection = this->newConnection(socket);
         connection->startListening();
         for (int i = 0; i < 8; ++i)
            socket->receive(Common::MessageHeader::GUI_LOCAL_BROWSE, request);
         delete connection;
      }
      for (int i = 0; i < 40; ++i)
      {
         jobs << RCM::localBrowse(request);
         QVERIFY(!jobs.last().future.isFinished()); // All global queue slots are available again.
      }
   }

   void localBrowseDoesNotBlockConnection()
   {
      QFETCH(bool, overload);
      QSemaphore started;
      QSemaphore release;
      auto& pool = RCM::localBrowsePool();
      for (int i = 0; i < 2; ++i)
         pool.start([&] { started.release(); release.acquire(); });
      const auto unblock = qScopeGuard([&] { release.release(2); pool.waitForDone(); });
      QVERIFY(started.tryAcquire(2, 3000));

      auto socket = new BufferedSocket;
      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      connection->startListening();
      QSignalSpy languageDefined(connection, &RCM::RemoteConnection::languageDefined);
      socket->output.clear();
      Protos::GUI::LocalBrowse request;
      request.set_path(this->dataDirectory.path().toStdString());
      socket->receive(Common::MessageHeader::GUI_LOCAL_BROWSE, request);
      QVERIFY(socket->output.isEmpty());
      socket->receive(Common::MessageHeader::GUI_LANGUAGE, Protos::GUI::Language());
      QCOMPARE(languageDefined.size(), 1); // Process another command while all browse workers are busy.
      if (overload)
      {
         for (int i = 0; i < 8; ++i)
            socket->receive(Common::MessageHeader::GUI_LOCAL_BROWSE, request);
         QVERIFY(!connection->isConnected());
      }
      else
         socket->close();
      QTRY_VERIFY(!connection); // Destruction must not wait for the filesystem workers.
   }

   void searchesExpireWithoutAnotherRequest()
   {
      SETTINGS.set("search_lifetime", quint32(30));
      auto network = QSharedPointer<NetworkListener>::create();
      auto socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket, network));
      connection->startListening();
      socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
      QCOMPARE(network->searches.size(), 1);
      QVERIFY(!network->searches[0].isNull());
      QTRY_VERIFY(network->searches[0].isNull());
      QVERIFY(connection->isConnected());
   }

   void expiredSearchResultsAreIgnored()
   {
      SETTINGS.set("search_lifetime", quint32(30));
      auto network = QSharedPointer<NetworkListener>::create();
      auto socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket, network));
      connection->startListening();
      socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
      auto search = network->searches[0].toStrongRef();
      QVERIFY(search);
      search->forcedElapsed = 0;
      socket->output.clear();
      search->deliver();
      QCOMPARE(socket->messages().size(), 1);
      QCOMPARE(socket->messages()[0].getHeader().getType(), Common::MessageHeader::GUI_SEARCH_RESULT);
      QCOMPARE(socket->messages()[0].getMessage<Protos::Common::FindResult>().tag(), search->tag);

      // Simulate expiry before the timer callback has had a chance to run.
      search->forcedElapsed = 30;
      search->deliver();
      QCOMPARE(socket->messages().size(), 1);
      QTest::qWait(80);
      socket->output.clear();
      // The timer must also disconnect a search retained by another owner.
      search->forcedElapsed = 0;
      search->deliver();
      QVERIFY(socket->output.isEmpty());
   }

   void searchLimitAndFailedLaunches()
   {
      SETTINGS.set("search_lifetime", quint32(30));
      auto network = QSharedPointer<NetworkListener>::create();
      auto socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket, network));
      connection->startListening();
      socket->output.clear();
      network->failSearch = true;
      socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
      QCOMPARE(socket->messages().last().getMessage<Protos::GUI::Tag>().tag(), quint64(0));
      QVERIFY(network->searches[0].isNull());
      network->failSearch = false;

      for (int i = 0; i < 100; ++i)
      {
         socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
         QVERIFY(socket->messages().last().getMessage<Protos::GUI::Tag>().tag() != 0);
      }
      socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
      QCOMPARE(socket->messages().last().getMessage<Protos::GUI::Tag>().tag(), quint64(0));
      QCOMPARE(network->searches.size(), 101); // Failed launch plus 100 successful launches.

      QTRY_VERIFY(network->searches.last().isNull());
      socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
      QVERIFY(socket->messages().last().getMessage<Protos::GUI::Tag>().tag() != 0);
      QVERIFY(!network->searches.last().isNull());
      connection.reset();
      QVERIFY(network->searches.last().isNull());
      QTest::qWait(80); // Pending expiry callbacks must be cancelled with the connection.
   }

   void uploadProgress()
   {
      QFETCH(quint64, size);
      QFETCH(quint64, owned);
      QFETCH(int, offset);
      QFETCH(int, expected);

      const PM::GetChunkParams params({}, offset, 0, owned);
      QCOMPARE(params.getFileBytesOwnedByPeer(), owned);
      QCOMPARE(RCM::uploadProgress(size, params.getFileBytesOwnedByPeer(), params.getOffset()), expected);
   }
private:
   QTemporaryDir dataDirectory;

   RCM::RemoteConnection* newConnection(BufferedSocket* socket, QSharedPointer<NL::INetworkListener> network = {})
   {
      auto files = QSharedPointer<FileManager>::create();
      return new RCM::RemoteConnection(files, PM::Builder::newPeerManager(files),
         QSharedPointer<UploadManager>::create(), QSharedPointer<DownloadManager>::create(),
         network, QSharedPointer<ChatSystem>::create(), socket);
   }
};

QTEST_GUILESS_MAIN(Tests)
#include "Tests.moc"
