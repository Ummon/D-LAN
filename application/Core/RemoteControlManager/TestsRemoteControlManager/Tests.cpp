#include <limits>
#include <stdexcept>

#include <QTest>
#include <QPointer>
#include <QSignalSpy>
#include <QTemporaryDir>
#include <QSemaphore>
#include <QScopeGuard>
#include <priv/LocalBrowse.h>

#include <Common/Constants.h>
#include <Common/Settings.h>
#include <Common/SaltedPassword.h>
#include <Common/Global.h>
#include <Common/ProtoHelper.h>
#include <Common/Network/RemoteControlAuthentication.h>
#include <Common/TestsCommon/GlobalRandomPredictor.h>
#include <Core/PeerManager/Builder.h>
#include <Core/DownloadManager/IDownload.h>
#include <Protos/core_settings.pb.h>
#include "Mocks.h"

#include <Core/PeerManager/GetChunkParams.h>
#include <priv/UploadProgress.h>

#if defined(Q_OS_WIN32)
   #ifndef NOMINMAX
      #define NOMINMAX
   #endif
   #include <windows.h>
#endif

namespace RCA = Common::RemoteControlAuthentication;

// See 'Protos.GUI.AskForAuthentication'. The test sockets don't use TLS: the channel binding is empty.
static Protos::GUI::Authentication authentication(quint64 challenge, const QByteArray& key, const QByteArray& nonce = RCA::randomBytes(RCA::NONCE_SIZE))
{
   Protos::GUI::Authentication authentication;
   authentication.set_protocol_version(RCA::PROTOCOL_VERSION);
   authentication.set_client_nonce(nonce.toStdString());
   authentication.set_client_proof(RCA::clientProof(key, challenge, nonce, QByteArray()).toStdString());
   return authentication;
}

static Protos::GUI::ChangePassword changePassword(const Common::SaltedPassword& password)
{
   Protos::GUI::ChangePassword request;
   request.set_new_salt(password.salt);
   request.mutable_new_kdf()->set_salt(password.kdfSalt.toStdString());
   request.mutable_new_kdf()->set_memory(password.kdfMemory);
   request.mutable_new_kdf()->set_iterations(password.kdfIterations);
   request.set_new_key(password.key.toStdString());
   return request;
}

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
      this->password = Common::SaltedPassword::create("password");
      SETTINGS.set("remote_password", this->password.toStr());
   }

   void init()
   {
      SETTINGS.set("remote_password", this->password.toStr());
   }

   void macOSInterfaceState()
   {
#ifndef Q_OS_MACOS
      QSKIP("macOS interface policy");
#else
      const QString originalAddress = SETTINGS.get<QString>("listen_address");
      const auto restore = qScopeGuard([&]() { SETTINGS.set("listen_address", originalAddress); });
      QString selectedAddress;
      for (const auto& interface : QNetworkInterface::allInterfaces())
         if (interface.name().startsWith("utun"))
            for (const auto& entry : interface.addressEntries())
               if (!entry.ip().scopeId().isEmpty())
                  selectedAddress = entry.ip().toString();
      SETTINGS.set("listen_address", selectedAddress);
      auto* socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket));
      connection->startListening();
      socket->output.clear();
      socket->receive(Common::MessageHeader::GUI_REFRESH, Protos::Common::Null());
      const auto messages = socket->messages();
      QCOMPARE(messages.size(), 1);
      const auto& state = messages[0].getMessage<Protos::GUI::State>();
      int listened = 0;
      for (const auto& reported : state.interfaces())
         for (const auto& address : reported.addresses())
         {
            QCOMPARE(address.listened(), QString::fromStdString(address.address()) == selectedAddress);
            listened += address.listened();
         }
      if (!selectedAddress.isEmpty())
         QCOMPARE(listened, 1);
      for (const auto& interface : QNetworkInterface::allInterfaces())
      {
         const QString name = interface.name();
         const bool auxiliary = name.startsWith("awdl") || name.startsWith("llw");
         const bool tunnel = name.startsWith("utun") || name.startsWith("gif") || name.startsWith("stf") ||
            interface.flags().testFlag(QNetworkInterface::IsPointToPoint);
         bool found = false;
         for (const auto& reported : state.interfaces())
            if (reported.id() == quint32(interface.index()))
            {
               QVERIFY(!auxiliary);
               QCOMPARE(reported.is_tunnel(), tunnel);
               found = true;
            }
         if (!auxiliary && interface.isValid() && !interface.addressEntries().isEmpty() &&
             interface.flags().testFlag(QNetworkInterface::CanMulticast) &&
             !interface.flags().testFlag(QNetworkInterface::IsLoopBack))
            QVERIFY(found);
      }
#endif
   }

   void downloadStateWithoutHashes_data()
   {
      QTest::addColumn<bool>("directory");
      QTest::addColumn<bool>("shared");
      QTest::newRow("file-shared") << false << true;
      QTest::newRow("file-pending") << false << false;
      QTest::newRow("directory-shared") << true << true;
      QTest::newRow("directory-pending") << true << false;
   }

   void downloadStateWithoutHashes()
   {
      QFETCH(bool, directory);
      QFETCH(bool, shared);
      struct Download : DM::IDownload
      {
         Protos::Common::Entry entry;
         PM::IPeer* peer = nullptr;
         quint64 getID() const override { return 42; }
         Protos::Common::DownloadStatus getStatus() const override { return Protos::Common::DOWNLOADING; }
         quint64 getDownloadedBytes() const override { return 123; }
         PM::IPeer* getPeerSource() const override { return this->peer; }
         QSet<PM::IPeer*> getPeers() const override { return { this->peer }; }
         const Protos::Common::Entry& getLocalEntry() const override { return this->entry; }
      } download;
      auto& entry = download.entry;
      entry.set_type(directory ? Protos::Common::Entry::DIR : Protos::Common::Entry::FILE);
      entry.set_path("nested/path/");
      entry.set_name("entry");
      entry.set_size(1ULL << 40);
      entry.set_hidden(true);
      entry.set_exists(true);
      entry.set_is_empty(directory);
      if (shared)
      {
         entry.mutable_shared_entry()->mutable_id()->set_hash(std::string(Common::Hash::HASH_SIZE, 's'));
         entry.mutable_shared_entry()->set_shared_name("share");
         entry.mutable_shared_entry()->set_path("/shared/");
      }
      if (!directory)
         for (int i = 0; i < 16384; ++i)
            entry.add_chunks()->set_hash(std::string(Common::Hash::HASH_SIZE, 'h'));
      entry.GetReflection()->MutableUnknownFields(&entry)->AddVarint(100, 456);
      const auto original = entry.SerializeAsString();
      auto expected = entry;
      expected.clear_chunks();

      auto peers = QSharedPointer<BrowsePeerManager>::create();
      download.peer = &peers->peer;
      auto downloads = QSharedPointer<DownloadManager>::create();
      downloads->downloads << &download;
      auto* socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket, {}, peers, {}, downloads));
      connection->startListening();
      socket->output.clear();
      socket->receive(Common::MessageHeader::GUI_REFRESH, Protos::Common::Null());
      const auto messages = socket->messages();
      QCOMPARE(messages.size(), 1);
      QCOMPARE(messages[0].getHeader().getType(), Common::MessageHeader::GUI_STATE);
      const auto& state = messages[0].getMessage<Protos::GUI::State>();
      QCOMPARE(state.downloads_size(), 1);
      const auto& result = state.downloads(0);
      QCOMPARE(result.local_entry().SerializeAsString(), expected.SerializeAsString());
      QCOMPARE(entry.SerializeAsString(), original);
      QCOMPARE(result.id(), quint64(42));
      QCOMPARE(result.downloaded_bytes(), quint64(123));
      QCOMPARE(result.status(), Protos::Common::DOWNLOADING);
      QCOMPARE(result.peer_ids_size(), 1);
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
      QVERIFY(messages[1].getMessage<Protos::GUI::AuthenticationResult>().core_proof().empty()); // Only for a remote GUI.
      QCOMPARE(languageDefined.size(), 1);
      QCOMPARE(languageDefined[0][0].value<QLocale>(), QLocale("fr_CH"));

      connection->startListening();
      QCOMPARE(socket->messages().size(), 4);
      QTest::qWait(5500); // A successful buffered response must cancel the original deadline.
      QVERIFY(connection);
      QVERIFY(connection->isConnected());
      delete connection;
   }

   void peerBrowseLimit_data()
   {
      QTest::addColumn<bool>("timeout");
      QTest::newRow("completion-frees-slot") << false;
      QTest::newRow("timeout-frees-slot") << true;
   }

   void peerBrowseLimit()
   {
      QFETCH(bool, timeout);
      auto peers = QSharedPointer<BrowsePeerManager>::create();
      auto* socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket, {}, peers));
      connection->startListening();
      Protos::GUI::Browse request;
      const auto id = peers->peer.getID();
      request.mutable_peer_id()->set_hash(id.getData(), Common::Hash::HASH_SIZE);
      socket->output.clear();
      for (int i = 0; i < 32; ++i)
      {
         request.set_tag(0xFEDCBA9876543200ULL + i);
         socket->receive(Common::MessageHeader::GUI_BROWSE, request);
      }
      QCOMPARE(peers->peer.requests, 32);
      QVERIFY(socket->messages().isEmpty()); // No acknowledgement; all peer requests are pending.
      request.set_tag(0xFEDCBA9876543220ULL);
      socket->receive(Common::MessageHeader::GUI_BROWSE, request);
      QCOMPARE(peers->peer.requests, 32); // Reject before allocating another peer request/socket.
      auto messages = socket->messages();
      QCOMPARE(messages.size(), 1);
      QCOMPARE(messages[0].getHeader().getType(), Common::MessageHeader::GUI_BROWSE_RESULT);
      QCOMPARE(messages[0].getMessage<Protos::GUI::BrowseResult>().tag(), request.tag());
      QCOMPARE(messages[0].getMessage<Protos::GUI::BrowseResult>().entries_size(), 0);

      socket->output.clear();
      auto first = peers->peer.entries[0].toStrongRef();
      QVERIFY(first->started);
      if (timeout)
         emit first->timeout();
      else
      {
         first->complete();
         QCOMPARE(socket->messages().size(), 1);
         const auto result = socket->messages()[0].getMessage<Protos::GUI::BrowseResult>();
         QCOMPARE(result.tag(), quint64(0xFEDCBA9876543200ULL));
         QCOMPARE(result.entries(0).entries(0).name(), std::string("test entry"));
      }
      if (timeout)
         QVERIFY(socket->messages().isEmpty());
      first.clear();
      QVERIFY(peers->peer.entries[0].isNull());
      socket->receive(Common::MessageHeader::GUI_BROWSE, request);
      QCOMPARE(peers->peer.requests, 33);
      connection.reset();
      for (const auto& entry : peers->peer.entries)
         QVERIFY(entry.isNull());
   }

   void synchronousPeerBrowse_data()
   {
      QTest::addColumn<int>("mode");
      QTest::newRow("result") << 1;
      QTest::newRow("timeout") << 2;
      QTest::newRow("unavailable") << 3;
   }

   void synchronousPeerBrowse()
   {
      QFETCH(int, mode);
      auto peers = QSharedPointer<BrowsePeerManager>::create();
      peers->peer.mode = mode;
      auto* socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket, {}, peers));
      connection->startListening();
      Protos::GUI::Browse request;
      const auto id = peers->peer.getID();
      request.mutable_peer_id()->set_hash(id.getData(), Common::Hash::HASH_SIZE);
      for (int i = 0; i < 40; ++i)
      {
         socket->output.clear();
         request.set_tag(i == 0 ? 0 : std::numeric_limits<quint64>::max() - i);
         socket->receive(Common::MessageHeader::GUI_BROWSE, request);
         QCOMPARE(peers->peer.requests, i + 1);
         if (mode != 3)
            QVERIFY(peers->peer.entries.last().isNull());
         const auto messages = socket->messages();
         QCOMPARE(messages.size(), mode == 2 ? 0 : 1);
         if (mode != 2)
         {
            QCOMPARE(messages[0].getHeader().getType(), Common::MessageHeader::GUI_BROWSE_RESULT);
            const auto result = messages[0].getMessage<Protos::GUI::BrowseResult>();
            QCOMPARE(result.tag(), request.tag());
            QCOMPARE(result.entries_size(), mode == 1 ? 1 : 0);
         }
      }
   }

   void immediateBrowse_data()
   {
      QTest::addColumn<bool>("self");
      QTest::addColumn<quint64>("tag");
      for (bool self : {false, true})
         for (quint64 tag : {quint64(0), std::numeric_limits<quint64>::max()})
            QTest::newRow(qPrintable(QString("%1-%2").arg(self ? "self" : "unknown").arg(tag))) << self << tag;
   }

   void immediateBrowse()
   {
      QFETCH(bool, self);
      QFETCH(quint64, tag);
      auto peers = QSharedPointer<BrowsePeerManager>::create();
      auto* socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket, {}, peers));
      connection->startListening();
      socket->output.clear();
      Protos::GUI::Browse request;
      const auto id = self ? peers->getSelf()->getID() : Common::Hash::rand();
      request.mutable_peer_id()->set_hash(id.getData(), Common::Hash::HASH_SIZE);
      request.set_tag(tag);
      socket->receive(Common::MessageHeader::GUI_BROWSE, request);
      const auto messages = socket->messages();
      QCOMPARE(messages.size(), 1);
      QCOMPARE(messages[0].getHeader().getType(), Common::MessageHeader::GUI_BROWSE_RESULT);
      const auto result = messages[0].getMessage<Protos::GUI::BrowseResult>();
      QCOMPARE(result.tag(), tag);
      QCOMPARE(result.entries_size(), self ? 1 : 0);
      QCOMPARE(peers->peer.requests, 0);
   }

   void malformedPasswordChange_data()
   {
      QTest::addColumn<QString>("field");
      for (const char* field : {"missing-key", "short-key", "missing-kdf", "short-kdf-salt", "weak-memory", "weak-iterations", "huge-memory"})
         QTest::newRow(field) << QString(field);
   }

   void malformedPasswordChange()
   {
      QFETCH(QString, field);
      auto* socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket));
      connection->startListening();
      socket->output.clear();

      auto request = changePassword(Common::SaltedPassword::create("new"));
      if (field == "missing-key")
         request.clear_new_key();
      else if (field == "short-key")
         request.mutable_new_key()->pop_back();
      else if (field == "missing-kdf")
         request.clear_new_kdf();
      else if (field == "short-kdf-salt")
         request.mutable_new_kdf()->mutable_salt()->pop_back();
      else if (field == "weak-memory")
         request.mutable_new_kdf()->set_memory(RCA::KDF_MEMORY - 1);
      else if (field == "weak-iterations")
         request.mutable_new_kdf()->set_iterations(RCA::KDF_ITERATIONS - 1);
      else if (field == "huge-memory")
         request.mutable_new_kdf()->set_memory(RCA::MAX_KDF_MEMORY + 1);

      socket->receive(Common::MessageHeader::GUI_CHANGE_PASSWORD, request);
      QCOMPARE(SETTINGS.get<QString>("remote_password"), this->password.toStr());
      QVERIFY(socket->output.isEmpty());
   }

   void validPasswordChanges()
   {
      SETTINGS.rm("remote_password");
      auto* socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket));
      connection->startListening();

      const auto first = Common::SaltedPassword::create("first");
      socket->receive(Common::MessageHeader::GUI_CHANGE_PASSWORD, changePassword(first));
      QCOMPARE(SETTINGS.get<QString>("remote_password"), first.toStr());

      const auto second = Common::SaltedPassword::create("second");
      socket->receive(Common::MessageHeader::GUI_CHANGE_PASSWORD, changePassword(second));
      QCOMPARE(SETTINGS.get<QString>("remote_password"), second.toStr());

      Protos::GUI::ChangePassword removal = changePassword(second);
      removal.set_remove(true); // The other fields are ignored.
      socket->receive(Common::MessageHeader::GUI_CHANGE_PASSWORD, removal);
      QVERIFY(SETTINGS.get<QString>("remote_password").isEmpty());
   }

   void authorizedPasswordChange_data()
   {
      QTest::addColumn<bool>("local");
      QTest::newRow("local-client-is-trusted") << true;
      QTest::newRow("remote-client-has-proven-the-password") << false;
   }

   void authorizedPasswordChange()
   {
      QFETCH(bool, local);
      auto* socket = new BufferedSocket(local);
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket));
      connection->startListening();
      if (!local)
      {
         const auto challenge = socket->messages()[0].getMessage<Protos::GUI::AskForAuthentication>().salt_challenge();
         socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, authentication(challenge, this->password.key));
         QCOMPARE(socket->messages()[1].getMessage<Protos::GUI::AuthenticationResult>().status(), Protos::GUI::AuthenticationResult::AUTH_OK);
      }

      const auto replacement = Common::SaltedPassword::create("replacement");
      socket->receive(Common::MessageHeader::GUI_CHANGE_PASSWORD, changePassword(replacement));
      QCOMPARE(SETTINGS.get<QString>("remote_password"), replacement.toStr());
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
      const QByteArray nonce = RCA::randomBytes(RCA::NONCE_SIZE);
      if (early)
         socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, authentication(0, this->password.key, nonce));
      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      QList<Common::Message> finalMessages;
      connect(connection, &RCM::RemoteConnection::deleted, this, [&] { finalMessages = socket->messages(); });
      connection->startListening();
      const auto messages = socket->messages();
      QCOMPARE(messages.size(), 1);
      QCOMPARE(messages[0].getHeader().getType(), Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION);
      const auto challenge = messages[0].getMessage<Protos::GUI::AskForAuthentication>();
      QCOMPARE(challenge.protocol_version(), RCA::PROTOCOL_VERSION);
      QCOMPARE(challenge.salt(), this->password.salt);
      QCOMPARE(QByteArray::fromStdString(challenge.kdf().salt()), this->password.kdfSalt);
      QCOMPARE(challenge.kdf().memory(), this->password.kdfMemory);
      QCOMPARE(challenge.kdf().iterations(), this->password.kdfIterations);
      if (early)
      {
         QTRY_VERIFY(!connection);
         QCOMPARE(finalMessages.size(), 2);
         QCOMPARE(finalMessages[1].getMessage<Protos::GUI::AuthenticationResult>().status(), Protos::GUI::AuthenticationResult::AUTH_BAD_PASSWORD);
      }
      else
      {
         socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, authentication(challenge.salt_challenge(), this->password.key, nonce));
         QCOMPARE(socket->messages().size(), 4);
         const auto result = socket->messages()[1].getMessage<Protos::GUI::AuthenticationResult>();
         QCOMPARE(result.status(), Protos::GUI::AuthenticationResult::AUTH_OK);
         QCOMPARE(
            QByteArray::fromStdString(result.core_proof()),
            RCA::coreProof(this->password.key, challenge.salt_challenge(), nonce, QByteArray())
         );
         delete connection;
      }
   }

   void refusedRemoteAuthentication_data()
   {
      QTest::addColumn<QString>("variant");
      QTest::addColumn<int>("status");
      const int bad = Protos::GUI::AuthenticationResult::AUTH_BAD_PASSWORD;
      QTest::newRow("wrong-key") << QString("wrong-key") << bad;
      QTest::newRow("wrong-challenge") << QString("wrong-challenge") << bad;
      QTest::newRow("proof-for-another-nonce") << QString("other-nonce") << bad;
      QTest::newRow("missing-nonce") << QString("missing-nonce") << bad;
      QTest::newRow("proof-bound-to-a-certificate") << QString("binding") << bad; // As relayed by a man in the middle.
      QTest::newRow("outdated-gui") << QString("outdated") << int(Protos::GUI::AuthenticationResult::AUTH_PROTOCOL_OUTDATED);
      QTest::newRow("no-password-defined") << QString("no-password") << int(Protos::GUI::AuthenticationResult::AUTH_PASSWORD_NOT_DEFINED);
   }

   void refusedRemoteAuthentication()
   {
      QFETCH(QString, variant);
      QFETCH(int, status);
      if (variant == "no-password")
         SETTINGS.rm("remote_password");
      auto* socket = new BufferedSocket(false);
      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      QList<Common::Message> finalMessages;
      connect(connection, &RCM::RemoteConnection::deleted, this, [&] { finalMessages = socket->messages(); });
      connection->startListening();
      const auto ask = socket->messages()[0].getMessage<Protos::GUI::AskForAuthentication>();
      QCOMPARE(ask.has_kdf(), variant != "no-password");

      const QByteArray nonce = RCA::randomBytes(RCA::NONCE_SIZE);
      auto request = authentication(ask.salt_challenge(), this->password.key, nonce);
      if (variant == "wrong-key")
         request = authentication(ask.salt_challenge(), RCA::randomBytes(RCA::KEY_SIZE));
      else if (variant == "wrong-challenge")
         request = authentication(ask.salt_challenge() + 1, this->password.key);
      else if (variant == "other-nonce")
         request.set_client_nonce(RCA::randomBytes(RCA::NONCE_SIZE).toStdString());
      else if (variant == "missing-nonce")
         request.clear_client_nonce();
      else if (variant == "binding")
         request.set_client_proof(RCA::clientProof(this->password.key, ask.salt_challenge(), nonce, RCA::randomBytes(32)).toStdString());
      else if (variant == "outdated")
         request.clear_protocol_version();
      socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, request);

      QTRY_VERIFY(!connection);
      QCOMPARE(finalMessages.size(), 2);
      const auto result = finalMessages[1].getMessage<Protos::GUI::AuthenticationResult>();
      QCOMPARE(int(result.status()), status);
      QVERIFY(result.core_proof().empty());
   }

   void challengeIsUnpredictable()
   {
      GlobalRandomPredictor predictor;
      if (!predictor.followsGlobal())
         QSKIP("QRandomGenerator::global() is no longer a Mersenne Twister");

      auto* socket = new BufferedSocket(false);
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket));
      connection->startListening();
      const auto messages = socket->messages();
      QCOMPARE(messages.size(), 1);
      QVERIFY(!predictor.predicts(messages[0].getMessage<Protos::GUI::AskForAuthentication>().salt_challenge()));
   }

   void disconnectedDuringStartup_data()
   {
      QTest::addColumn<bool>("malformed");
      QTest::newRow("already-closed") << false;
      QTest::newRow("buffered-invalid-header") << true;
   }

   void repeatedAuthentication_data()
   {
      QTest::addColumn<bool>("local");
      QTest::newRow("trusted-local-client") << true;
      QTest::newRow("remote-client") << false;
   }

   void repeatedAuthentication()
   {
      QFETCH(bool, local);
      auto* socket = new BufferedSocket(local);
      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      connection->startListening();
      const auto challenge = socket->messages()[0].getMessage<Protos::GUI::AskForAuthentication>().salt_challenge();
      const auto valid = authentication(challenge, this->password.key);
      socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, valid);
      QCOMPARE(socket->messages().size(), 4);
      QCOMPARE(socket->messages()[1].getMessage<Protos::GUI::AuthenticationResult>().status(), Protos::GUI::AuthenticationResult::AUTH_OK);
      QSignalSpy languageDefined(connection, &RCM::RemoteConnection::languageDefined);
      socket->output.clear();

      socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, valid);
      socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, Protos::GUI::Authentication());
      socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, valid);
      QTest::qWait(50); // Beyond the configured failed-authentication delay.
      QVERIFY(connection);
      QVERIFY(connection->isConnected());
      QVERIFY(socket->output.isEmpty()); // No repeated authentication result, state, or history.
      socket->receive(Common::MessageHeader::GUI_LANGUAGE, Protos::GUI::Language());
      QCOMPARE(languageDefined.size(), 1);
      delete connection;
   }

   void refusedAuthenticationCannotBeRetried()
   {
      auto* socket = new BufferedSocket(false);
      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      connection->startListening();
      const auto challenge = socket->messages()[0].getMessage<Protos::GUI::AskForAuthentication>().salt_challenge();
      QList<Common::Message> finalMessages;
      connect(connection, &RCM::RemoteConnection::deleted, this, [&] { finalMessages = socket->messages(); });
      socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, authentication(challenge, RCA::randomBytes(RCA::KEY_SIZE)));
      socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, authentication(challenge, this->password.key));
      QVERIFY(connection->isConnected()); // Ignored until the delayed refusal, other messages are tested by 'unauthorizedHeaders'.
      QTRY_VERIFY(!connection);
      QCOMPARE(finalMessages.size(), 2);
      QCOMPARE(finalMessages[1].getMessage<Protos::GUI::AuthenticationResult>().status(), Protos::GUI::AuthenticationResult::AUTH_BAD_PASSWORD);
   }

   void unauthorizedHeaders_data()
   {
      QTest::addColumn<bool>("local");
      QTest::addColumn<bool>("refused");
      QTest::addColumn<int>("type");
      QTest::addColumn<quint32>("size");
      QTest::addColumn<bool>("accepted");

      const quint32 limit = Common::Constants::MAX_GUI_HANDSHAKE_MESSAGE_SIZE;
      const quint32 maximum = 100 * 1024 * 1024; // The largest payload accepted by 'Common::MessageSocket'.
      const int authentication = Common::MessageHeader::GUI_AUTHENTICATION;
      const int language = Common::MessageHeader::GUI_LANGUAGE;
      const int state = Common::MessageHeader::GUI_STATE;
      QTest::newRow("authentication") << false << false << authentication << limit << true;
      QTest::newRow("oversized-authentication") << false << false << authentication << limit + 1 << false;
      QTest::newRow("other-type") << false << false << language << quint32(0) << false;
      QTest::newRow("huge-other-type") << false << false << state << maximum << false;
      QTest::newRow("refused-authentication") << false << true << authentication << limit << true;
      QTest::newRow("refused-other-type") << false << true << language << quint32(0) << false;
      QTest::newRow("trusted-local-client") << true << false << state << maximum << true;
   }

   void unauthorizedHeaders()
   {
      QFETCH(bool, local);
      QFETCH(bool, refused);
      QFETCH(int, type);
      QFETCH(quint32, size);
      QFETCH(bool, accepted);
      auto* socket = new BufferedSocket(local);
      QPointer<RCM::RemoteConnection> connection = this->newConnection(socket);
      QList<Common::Message> finalMessages;
      connect(connection, &RCM::RemoteConnection::deleted, this, [&] { finalMessages = socket->messages(); });
      connection->startListening();
      if (refused)
         socket->receive(Common::MessageHeader::GUI_AUTHENTICATION, Protos::GUI::Authentication());

      socket->receiveHeader(Common::MessageHeader::MessageType(type), size);
      QCOMPARE(connection->isConnected(), accepted); // A rejection waits neither for the body nor for the authentication deadline.
      if (accepted)
         delete connection;
      else
      {
         QTRY_VERIFY(!connection);
         QCOMPARE(finalMessages.size(), 1); // Only the challenge, a pending refusal isn't sent either.
      }
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
      QVERIFY(root.mkdir(".hidden"));
      for (const auto& name : {"file.txt", "nonempty/child.txt", ".hidden.txt", "nonempty/.hidden.txt"})
      {
         QFile file(root.filePath(name));
         QVERIFY(file.open(QIODevice::WriteOnly));
         QCOMPARE(file.write("hello"), qint64(5));
      }
#ifdef Q_OS_WIN32
      // A leading dot doesn't hide an entry on Windows.
      for (const auto& name : {".hidden", ".hidden.txt", "nonempty/.hidden.txt"})
         QVERIFY(SetFileAttributesW(root.filePath(name).toStdWString().c_str(), FILE_ATTRIBUTE_HIDDEN));
#endif
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
      // The hidden entries are always sent, the GUI chooses whether to display them.
      QCOMPARE(result.entries_size(), 5);
      QMap<QString, qint64> sizes;
      for (const auto& entry : result.entries())
      {
         const QString name = QString::fromStdString(entry.name());
         sizes.insert(name, entry.size());
         QCOMPARE(entry.type(), name.endsWith(".txt") ? Protos::GUI::LocalBrowseResult::FILE : Protos::GUI::LocalBrowseResult::DIR);
         QCOMPARE(entry.hidden(), name.startsWith(".hidden"));
         QVERIFY(entry.date_modified() > 0);
      }
      QCOMPARE(sizes.value("empty", -1), qint64(0));
      QCOMPARE(sizes.value("nonempty", -1), qint64(2)); // The hidden children are counted.
      QCOMPARE(sizes.value("file.txt", -1), qint64(5));
      QCOMPARE(sizes.value(".hidden", -1), qint64(0));
      QCOMPARE(sizes.value(".hidden.txt", -1), qint64(5));

      // The roots are never hidden.
      request.clear_path();
      socket->output.clear();
      socket->receive(Common::MessageHeader::GUI_LOCAL_BROWSE, request);
      QTRY_COMPARE(socket->messages().size(), 1);
      const auto roots = socket->messages()[0].getMessage<Protos::GUI::LocalBrowseResult>();
      QVERIFY(roots.entries_size() > 0);
      for (const auto& entry : roots.entries())
         QVERIFY(!entry.hidden());
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

   void searchTags_data()
   {
      QTest::addColumn<bool>("local");
      QTest::addColumn<quint64>("tag");
      for (bool local : {false, true})
         for (quint64 tag : {quint64(0), std::numeric_limits<quint64>::max()})
            QTest::newRow(qPrintable(QString("%1-%2").arg(local ? "local" : "network").arg(tag))) << local << tag;
   }

   void searchTags()
   {
      QFETCH(bool, local);
      QFETCH(quint64, tag);
      SETTINGS.set("search_lifetime", quint32(5000));
      auto files = QSharedPointer<FileManager>::create();
      Protos::Common::FindResult original;
      original.set_tag(123);
      original.add_entries()->mutable_entry()->set_name("test file");
      files->searchResults << original;
      auto network = QSharedPointer<NetworkListener>::create();
      auto* socket = new BufferedSocket;
      QScopedPointer<RCM::RemoteConnection> connection(this->newConnection(socket, network, {}, files));
      connection->startListening();
      socket->output.clear();
      Protos::GUI::Search request;
      request.set_local(local);
      request.set_tag(tag);
      socket->receive(Common::MessageHeader::GUI_SEARCH, request);
      if (!local)
      {
         QVERIFY(socket->output.isEmpty()); // No tag acknowledgement.
         auto first = network->searches[0].toStrongRef();
         QVERIFY(first);
         request.set_tag(tag ^ 1);
         socket->receive(Common::MessageHeader::GUI_SEARCH, request);
         auto second = network->searches[1].toStrongRef();
         QVERIFY(second);
         second->deliver();
         QCOMPARE(socket->messages().size(), 1);
         QCOMPARE(socket->messages()[0].getMessage<Protos::Common::FindResult>().tag(), request.tag());
         socket->output.clear();
         // Multiple batches retain the GUI tag without changing the network result.
         emit first->found(original);
         emit first->found(original);
         QCOMPARE(original.tag(), quint64(123));
      }
      else
         QVERIFY(network->searches.isEmpty());

      const auto messages = socket->messages();
      QCOMPARE(messages.size(), local ? 1 : 2);
      for (const auto& message : messages)
      {
         QCOMPARE(message.getHeader().getType(), Common::MessageHeader::GUI_SEARCH_RESULT);
         const auto result = message.getMessage<Protos::Common::FindResult>();
         QCOMPARE(result.tag(), tag);
         QCOMPARE(result.entries_size(), 1);
         QCOMPARE(result.entries(0).entry().name(), std::string("test file"));
      }
      QCOMPARE(files->searchResults[0].tag(), quint64(123));
      if (local)
      {
         socket->output.clear();
         files->searchResults.clear();
         socket->receive(Common::MessageHeader::GUI_SEARCH, request);
         QVERIFY(socket->output.isEmpty());
      }
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
      QCOMPARE(socket->messages()[0].getMessage<Protos::Common::FindResult>().tag(), quint64(0));

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
      QVERIFY(socket->output.isEmpty());
      QVERIFY(network->searches[0].isNull());
      network->failSearch = false;

      for (int i = 0; i < 100; ++i)
      {
         socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
         QVERIFY(socket->output.isEmpty());
         QVERIFY(!network->searches.last().isNull());
      }
      socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
      QVERIFY(socket->output.isEmpty());
      QCOMPARE(network->searches.size(), 101); // Failed launch plus 100 successful launches.

      QTRY_VERIFY(network->searches.last().isNull());
      socket->receive(Common::MessageHeader::GUI_SEARCH, Protos::GUI::Search());
      QVERIFY(socket->output.isEmpty());
      QCOMPARE(network->searches.size(), 102);
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
   Common::SaltedPassword password; // Defined before each test, its plain form is "password".

   RCM::RemoteConnection* newConnection(BufferedSocket* socket, QSharedPointer<NL::INetworkListener> network = {},
      QSharedPointer<PM::IPeerManager> peers = {}, QSharedPointer<FileManager> files = {},
      QSharedPointer<DownloadManager> downloads = {})
   {
      if (!files)
         files = QSharedPointer<FileManager>::create();
      return new RCM::RemoteConnection(files, peers ? peers : PM::Builder::newPeerManager(files),
         QSharedPointer<UploadManager>::create(), downloads ? downloads : QSharedPointer<DownloadManager>::create(),
         network, QSharedPointer<ChatSystem>::create(), socket, Common::Global::isLocal(socket->peerAddress()));
   }
};

QTEST_GUILESS_MAIN(Tests)
#include "Tests.moc"
