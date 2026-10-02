#include <memory>

#include <QTest>
#include <QSignalSpy>
#include <QTemporaryDir>
#include <QFile>
#include <QDir>
#include <QSslKey>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/SaltedPassword.h>
#include <Common/Network/MessageSocket.h>
#include <Common/Network/RemoteControlAuthentication.h>
#include <Common/Network/RemoteControlTls.h>
#include <Common/RemoteCoreController/priv/InternalCoreConnection.h>
#include <Core/RemoteControlManager/priv/RemoteControlManager.h>
#include <Core/PeerManager/Builder.h>
#include <Protos/core_settings.pb.h>
#include "Mocks.h"

namespace Tls = Common::RemoteControlTls;
namespace RCA = Common::RemoteControlAuthentication;

// Use real loopback TCP/TLS, changing only the server's perceived peer address
// so remote authentication and transport policy can be exercised on one host.
class TestSocket : public QSslSocket
{
public:
   using QSslSocket::QSslSocket;
   void remotePeer() { this->setPeerAddress(QHostAddress("192.0.2.1")); }
};

class TestServer : public QTcpServer
{
public:
   bool remote = true;
protected:
   void incomingConnection(qintptr descriptor) override
   {
      auto* socket = new TestSocket(this);
      if (!socket->setSocketDescriptor(descriptor))
      {
         delete socket;
         return;
      }
      if (this->remote)
         socket->remotePeer();
      this->addPendingConnection(socket);
   }
};

class TestConnection : public RCC::InternalCoreConnection
{
public:
   using InternalCoreConnection::InternalCoreConnection;
   QSslSocket* transport() { return static_cast<QSslSocket*>(this->socket); }
};

// A man in the middle: terminates the TLS connection of a GUI with the given identity and relays it to the core.
class Relay : public QTcpServer
{
public:
   Relay(const QSslConfiguration& identity, quint16 corePort) : identity(identity), corePort(corePort) {}

protected:
   void incomingConnection(qintptr descriptor) override
   {
      auto* gui = new QSslSocket(this);
      auto* core = new QSslSocket(this);
      if (!gui->setSocketDescriptor(descriptor))
         return;
      connect(gui, &QSslSocket::readyRead, core, [gui, core] { core->write(gui->readAll()); });
      connect(core, &QSslSocket::readyRead, gui, [gui, core] { gui->write(core->readAll()); });
      connect(gui, &QSslSocket::disconnected, core, &QSslSocket::disconnectFromHost);
      connect(core, &QSslSocket::disconnected, gui, &QSslSocket::disconnectFromHost);
      gui->setSslConfiguration(this->identity);
      gui->startServerEncryption();
      auto configuration = QSslConfiguration::defaultConfiguration();
      configuration.setPeerVerifyMode(QSslSocket::VerifyNone);
      core->setSslConfiguration(configuration);
      core->connectToHostEncrypted("127.0.0.1", this->corePort);
   }

private:
   const QSslConfiguration identity;
   const quint16 corePort;
};

// Sends and records raw protocol messages, as an impostor core.
class Peer : public Common::MessageSocket
{
   class Logger : public ILogger
   {
      void logDebug(const QString&) override {}
      void logError(const QString&) override {}
   };

public:
   explicit Peer(QAbstractSocket* socket) : MessageSocket(new Logger, socket, Common::Hash::rand()) { this->startListening(); }
   QList<Common::Message> received;

private:
   void onNewMessage(const Common::Message& message) override { this->received << message; }
};

class Tests : public QObject
{
   Q_OBJECT
   std::unique_ptr<QTemporaryDir> directory;
   std::unique_ptr<RCM::RemoteControlManager> manager;
   TestServer server;
   RCC::CoreController controller;
   Common::SaltedPassword password; // Of the core, its plain form is "password".

   QString identityPath() const { return this->directory->filePath("remote-control-tls/core.pem"); }

   void startManager()
   {
      auto files = QSharedPointer<FileManager>::create();
      this->manager = std::make_unique<RCM::RemoteControlManager>(files, PM::Builder::newPeerManager(files),
         QSharedPointer<UploadManager>::create(), QSharedPointer<DownloadManager>::create(),
         QSharedPointer<NetworkListener>::create(), QSharedPointer<ChatSystem>::create());
      connect(&this->server, &QTcpServer::newConnection, this->manager.get(), &RCM::RemoteControlManager::newConnection);
   }

   // 'port': of the core by default.
   void connectClient(TestConnection& client, bool remote = true, bool correctPassword = true, quint16 port = 0)
   {
      client.connectionInfo.address = "test-core";
      client.connectionInfo.port = port != 0 ? port : this->server.serverPort();
      client.connectionInfo.password = this->password;
      if (!correctPassword)
         client.connectionInfo.password.key = RCA::randomBytes(RCA::KEY_SIZE);
      client.tlsRequired = remote;
      client.tlsFailureReported = false;
      client.connectionAttemptActive = true;
      client.stopListening();
      client.connectionTimeoutTimer.start();
      if (remote)
      {
         auto configuration = QSslConfiguration::defaultConfiguration();
         configuration.setProtocol(QSsl::TlsV1_2OrLater);
         configuration.setPeerVerifyMode(QSslSocket::VerifyPeer);
         client.transport()->setSslConfiguration(configuration);
         client.transport()->ignoreSslErrors(QList<QSslError>());
         client.transport()->connectToHostEncrypted("127.0.0.1", client.connectionInfo.port, "test-core");
      }
      else
         client.transport()->connectToHost(QHostAddress::LocalHost, client.connectionInfo.port);
   }

   // Like the last attempt of 'tryToConnectToTheNextAddress()': its failure is reported as a connecting error.
   void reportFinalError(TestConnection& client)
   {
      connect(client.transport(), &QAbstractSocket::stateChanged, &client, &RCC::InternalCoreConnection::stateChanged);
      client.nbRetries = RCC::InternalCoreConnection::NB_RETRIES_MAX;
   }

private slots:
   void initTestCase()
   {
      const QString backend = qEnvironmentVariable("DLAN_TEST_TLS_BACKEND");
      if (!backend.isEmpty())
         QVERIFY2(QSslSocket::setActiveBackend(backend), qPrintable(backend));
      this->password = Common::SaltedPassword::create("password");
   }

   void init()
   {
      this->directory = std::make_unique<QTemporaryDir>();
      QVERIFY(this->directory->isValid());
      Common::Global::setDataFolder(Common::Global::DataFolderType::ROAMING, this->directory->path());
      auto* settings = new Protos::Core::Settings;
      settings->set_remote_control_port(0);
      settings->set_remote_max_nb_connection(4);
      settings->set_remote_refresh_rate(1000);
      settings->set_delay_before_sending_log_messages(100);
      settings->set_delay_gui_connection_fail(10);
      settings->set_peer_timeout_factor(3);
      settings->set_peer_imalive_period(5000);
      SETTINGS.setSettingsMessage(settings);
      SETTINGS.set("peer_id", Common::Hash::rand());
      SETTINGS.set("remote_password", this->password.toStr());
      this->server.remote = true;
      QVERIFY(this->server.listen(QHostAddress::LocalHost));
      QVERIFY2(QSslSocket::supportsSsl(), "The integration tests require a deployed Qt TLS backend");
   }

   void cleanup()
   {
      this->server.close();
      this->manager.reset();
      QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
      Common::Global::setDataFolderToDefault(Common::Global::DataFolderType::ROAMING);
      this->directory.reset();
   }

   void identityPersists()
   {
      const auto first = Tls::serverConfiguration();
      QVERIFY(!first.localCertificate().isNull());
      QVERIFY(!first.privateKey().isNull());
      QCOMPARE(first.privateKey().length(), 3072);
      const QStringList commonName = first.localCertificate().subjectInfo(QSslCertificate::CommonName);
      QCOMPARE(commonName.size(), 1);
      QVERIFY(commonName.first().startsWith("D-LAN Core "));
      QCOMPARE(first.localCertificate().issuerInfo(QSslCertificate::CommonName), commonName);
      QCOMPARE(first.localCertificate().subjectInfo(QSslCertificate::Organization), QStringList("D-LAN Core"));
      QCOMPARE(first.localCertificate().subjectInfo(QSslCertificate::Organization),
         first.localCertificate().issuerInfo(QSslCertificate::Organization));
      QVERIFY(first.localCertificate().expiryDate() > QDateTime::currentDateTimeUtc().addYears(9));
      const auto second = Tls::serverConfiguration();
      QCOMPARE(second.localCertificate(), first.localCertificate());
      QCOMPARE(second.privateKey().toDer(), first.privateKey().toDer());
      QVERIFY(QFile::exists(this->identityPath()));
#ifdef Q_OS_UNIX
      QCOMPARE(QFile::permissions(this->identityPath()) &
         (QFileDevice::ReadGroup | QFileDevice::WriteGroup | QFileDevice::ReadOther | QFileDevice::WriteOther), QFileDevice::Permissions());
#endif
   }

   void encryptedLoginAndReconnect()
   {
      this->startManager();
      TestConnection client(this->controller);
      QSignalSpy state(&client, &RCC::InternalCoreConnection::newState);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      this->connectClient(client);
      QTRY_VERIFY(client.isConnected());
      QVERIFY(client.transport()->isEncrypted());
      QTRY_VERIFY(!state.isEmpty());
      const QString path = Tls::pinPath("test-core", this->server.serverPort());
      QFile pin(path);
      QVERIFY(pin.open(QIODevice::ReadOnly));
      QCOMPARE(QSslCertificate(pin.readAll()), this->manager->tlsConfiguration.localCertificate());
      pin.close();
      client.disconnectFromCore();
      QTRY_VERIFY(this->manager->connections.isEmpty());
      this->connectClient(client);
      QTRY_VERIFY(client.isConnected());
      QVERIFY(client.transport()->isEncrypted());
      QCOMPARE(errors.size(), 0);
      QCOMPARE(Tls::pinPath("TEST-CORE.", this->server.serverPort()), path);
   }

   void localPlaintextAfterTls()
   {
      this->startManager();
      TestConnection client(this->controller);
      this->connectClient(client);
      QTRY_VERIFY(client.isConnected());
      client.disconnectFromCore();
      QTRY_VERIFY(this->manager->connections.isEmpty());
      this->server.remote = false;
      this->connectClient(client, false, false); // Local connections do not need a password.
      QTRY_VERIFY(client.isConnected());
      QVERIFY(!client.transport()->isEncrypted());
   }

   void wrongPasswordDoesNotPin()
   {
      this->startManager();
      TestConnection client(this->controller);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      this->connectClient(client, true, false);
      QTRY_COMPARE(errors.size(), 1);
      QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::RCC_ERROR_WRONG_PASSWORD);
      QVERIFY(!QFile::exists(Tls::pinPath("test-core", this->server.serverPort())));
      QVERIFY(!client.isConnected());
   }

   void loginWithEveryCredential_data()
   {
      QTest::addColumn<QString>("credential");
      for (const char* credential : {"key", "plain-password", "legacy-hash"})
         QTest::newRow(credential) << QString(credential);
   }

   // The key saved after a previous connection, a typed password, or the hash saved by a version prior to 1.4.2.
   void loginWithEveryCredential()
   {
      QFETCH(QString, credential);
      this->startManager();
      TestConnection client(this->controller);
      this->connectClient(client);
      // No event has been processed yet: the core hasn't asked for the authentication.
      if (credential == "plain-password")
      {
         client.connectionInfo.password = Common::SaltedPassword();
         client.password = "password";
      }
      else if (credential == "legacy-hash")
      {
         client.connectionInfo.password = Common::SaltedPassword();
         client.connectionInfo.password.legacyHash = Common::Hasher::hashWithSalt(QString("password"), this->password.salt);
      }

      QTRY_VERIFY(client.isConnected());
      // The derived key is kept, the GUI saves it for the next connections.
      QVERIFY(client.getConnectionInfo().password.sameDerivation(this->password));
      QCOMPARE(client.getConnectionInfo().password.key, this->password.key);
      QVERIFY(client.password.isEmpty());
   }

   void relayedLogin_data()
   {
      QTest::addColumn<bool>("coreCertificate");
      QTest::newRow("relay-with-the-certificate-of-the-core") << true; // Checks the relay itself.
      QTest::newRow("man-in-the-middle") << false;
   }

   void relayedLogin()
   {
      QFETCH(bool, coreCertificate);
      this->startManager();
      if (!coreCertificate)
         QVERIFY(QFile::remove(this->identityPath())); // A new identity is generated for the relay.
      Relay relay(coreCertificate ? this->manager->tlsConfiguration : Tls::serverConfiguration(), this->server.serverPort());
      QVERIFY(relay.listen(QHostAddress::LocalHost));

      TestConnection client(this->controller);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      this->connectClient(client, true, true, relay.serverPort());
      if (coreCertificate)
      {
         QTRY_VERIFY(client.isConnected());
         QCOMPARE(errors.size(), 0);
         return;
      }

      // The proof of the GUI is bound to the certificate of the relay: the core refuses it.
      QTRY_COMPARE(errors.size(), 1);
      QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::RCC_ERROR_WRONG_PASSWORD);
      QVERIFY(!client.isConnected());
      QVERIFY(!QFile::exists(Tls::pinPath("test-core", relay.serverPort())));
   }

   void impostorCoreRejected_data()
   {
      QTest::addColumn<QString>("variant");
      QTest::addColumn<int>("error");
      const int notAuthenticated = RCC::ICoreConnection::RCC_ERROR_CORE_NOT_AUTHENTICATED;
      const int incompatible = RCC::ICoreConnection::RCC_ERROR_INCOMPATIBLE_VERSION;
      QTest::newRow("unsolicited-ok") << QString("unsolicited-ok") << notAuthenticated;
      QTest::newRow("ok-without-proof") << QString("no-proof") << notAuthenticated;
      QTest::newRow("ok-with-a-wrong-proof") << QString("wrong-proof") << notAuthenticated;
      QTest::newRow("core-of-a-prior-version") << QString("old-core") << incompatible;
      QTest::newRow("weak-key-derivation") << QString("weak-kdf") << incompatible;
   }

   // A server which knows the salts and the key derivation parameters of the core, but not its password.
   void impostorCoreRejected()
   {
      QFETCH(QString, variant);
      QFETCH(int, error);
      TestConnection client(this->controller);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      this->connectClient(client);
      QTRY_VERIFY(this->server.hasPendingConnections());
      auto* socket = static_cast<QSslSocket*>(this->server.nextPendingConnection());
      QSignalSpy encrypted(socket, &QSslSocket::encrypted);
      socket->setSslConfiguration(Tls::serverConfiguration());
      socket->startServerEncryption();
      QTRY_COMPARE(encrypted.size(), 1);
      Peer impostor(socket);

      if (variant != "unsolicited-ok")
      {
         Protos::GUI::AskForAuthentication ask;
         if (variant != "old-core")
            ask.set_protocol_version(RCA::PROTOCOL_VERSION);
         ask.set_salt(this->password.salt);
         ask.set_salt_challenge(42);
         ask.mutable_kdf()->set_salt(this->password.kdfSalt.toStdString());
         ask.mutable_kdf()->set_memory(variant == "weak-kdf" ? RCA::KDF_MEMORY / 2 : this->password.kdfMemory);
         ask.mutable_kdf()->set_iterations(this->password.kdfIterations);
         impostor.send(Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION, ask);
      }

      if (error == RCC::ICoreConnection::RCC_ERROR_CORE_NOT_AUTHENTICATED)
      {
         if (variant != "unsolicited-ok")
            QTRY_COMPARE(impostor.received.size(), 1); // The proof of the GUI.
         Protos::GUI::AuthenticationResult result;
         result.set_status(Protos::GUI::AuthenticationResult::AUTH_OK);
         if (variant == "wrong-proof")
            result.set_core_proof(RCA::randomBytes(32).toStdString());
         impostor.send(Common::MessageHeader::GUI_AUTHENTICATION_RESULT, result);
      }

      QTRY_COMPARE(errors.size(), 1);
      QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::ConnectionErrorCode(error));
      QVERIFY(!client.isConnected());
      QVERIFY(!QFile::exists(Tls::pinPath("test-core", this->server.serverPort())));
      if (error == RCC::ICoreConnection::RCC_ERROR_INCOMPATIBLE_VERSION)
      {
         QTest::qWait(100);
         QVERIFY(impostor.received.isEmpty()); // No proof to crack offline.
      }
   }

   void remoteAccessRequiresPassword()
   {
      SETTINGS.rm("remote_password");
      this->startManager();
      TestConnection client(this->controller);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      this->reportFinalError(client);
      this->connectClient(client);
      QTRY_COMPARE(errors.size(), 1);
      QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::RCC_ERROR_CLOSED_BY_CORE);
      QVERIFY(this->manager->connections.isEmpty()); // Refused before TLS, without taking a connection slot.
      QVERIFY(!QFile::exists(Tls::pinPath("test-core", this->server.serverPort())));

      this->server.remote = false;
      TestConnection local(this->controller);
      this->connectClient(local, false, false);
      QTRY_VERIFY(local.isConnected()); // Local access doesn't need a password.
      local.disconnectFromCore();
      QTRY_VERIFY(this->manager->connections.isEmpty());

      // A password defined while the core runs enables remote access.
      this->server.remote = true;
      SETTINGS.set("remote_password", this->password.toStr());
      TestConnection authorized(this->controller);
      this->connectClient(authorized);
      QTRY_VERIFY(authorized.isConnected());
      QVERIFY(authorized.transport()->isEncrypted());
   }

   void silentRemoteCoreTimesOut()
   {
      // The TCP connection is accepted but no core answers: it's a timeout, not a refusal.
      TestConnection client(this->controller);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      this->reportFinalError(client);
      client.connectionTimeoutTimer.setInterval(100);
      this->connectClient(client);
      QTRY_VERIFY(this->server.hasPendingConnections());
      QScopedPointer<QTcpSocket> idle(this->server.nextPendingConnection());
      QTRY_COMPARE(errors.size(), 1);
      QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::RCC_ERROR_HOST_TIMEOUT);
   }

   void changedCertificateRejected()
   {
      this->startManager();
      TestConnection client(this->controller);
      this->connectClient(client);
      QTRY_VERIFY(client.isConnected());
      client.disconnectFromCore();
      QTRY_VERIFY(this->manager->connections.isEmpty());
      QVERIFY(QFile::remove(this->identityPath()));
      this->manager->tlsConfiguration = Tls::serverConfiguration();
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      QSignalSpy messages(&client, &Common::MessageSocket::newMessage);
      this->connectClient(client);
      QTRY_COMPARE(errors.size(), 1);
      QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::RCC_ERROR_TLS);
      QVERIFY(!client.isConnected());
      QCOMPARE(messages.size(), 0); // No authentication challenge is processed.
   }

   void corruptIdentityKeepsLocalAccess()
   {
      Tls::serverConfiguration();
      QFile identity(this->identityPath());
      QVERIFY(identity.open(QIODevice::WriteOnly | QIODevice::Truncate));
      identity.write("broken identity");
      identity.close();
      QVERIFY_THROWS_EXCEPTION(QString, Tls::serverConfiguration());
      this->startManager();
      QVERIFY(this->manager->tlsConfiguration.isNull());
      this->server.remote = false;
      TestConnection client(this->controller);
      this->connectClient(client, false);
      QTRY_VERIFY(client.isConnected());
      QVERIFY(!client.transport()->isEncrypted());
      QVERIFY(identity.open(QIODevice::ReadOnly));
      QCOMPARE(identity.readAll(), QByteArray("broken identity"));
   }

   void mismatchedIdentityRejected()
   {
      const auto first = Tls::serverConfiguration();
      QVERIFY(QFile::remove(this->identityPath()));
      const auto second = Tls::serverConfiguration();
      QFile identity(this->identityPath());
      QVERIFY(identity.open(QIODevice::WriteOnly | QIODevice::Truncate));
      const auto mismatched = first.privateKey().toPem() + second.localCertificate().toPem();
      QCOMPARE(identity.write(mismatched), mismatched.size());
      identity.close();
      QVERIFY_THROWS_EXCEPTION(QString, Tls::serverConfiguration());
   }

   void persistenceFailureAfterHandshake()
   {
      this->startManager();
      TestConnection client(this->controller);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      // encrypted() has already validated the unknown peer. Fail only when
      // committing trust after AUTH_OK, not during certificate validation.
      connect(client.transport(), &QSslSocket::encrypted, &client, [this] {
         QDir().mkpath(Tls::pinPath("test-core", this->server.serverPort()) + ".lock");
      });
      this->connectClient(client);
      QTRY_COMPARE_WITH_TIMEOUT(errors.size(), 1, 10000);
      QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::RCC_ERROR_TLS);
      QVERIFY(!client.isConnected());
      QVERIFY(!QFile::exists(Tls::pinPath("test-core", this->server.serverPort())));
   }

   void pinPersistenceFailureRejectsLogin()
   {
      this->startManager();
      // A directory where the pin file should be makes persistence fail.
      QVERIFY(QDir().mkpath(Tls::pinPath("test-core", this->server.serverPort())));
      TestConnection client(this->controller);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      this->connectClient(client);
      QTRY_COMPARE(errors.size(), 1);
      QCOMPARE(errors[0][0].value<RCC::ICoreConnection::ConnectionErrorCode>(), RCC::ICoreConnection::RCC_ERROR_TLS);
      QVERIFY(!client.isConnected());
   }

   void pendingTlsIsBounded()
   {
      SETTINGS.set("remote_max_nb_connection", quint32(1));
      this->startManager();
      QTcpSocket stalled;
      stalled.connectToHost(QHostAddress::LocalHost, this->server.serverPort());
      QTRY_COMPARE(this->manager->connections.size(), 1);
      QCOMPARE(stalled.bytesAvailable(), 0); // No plaintext authentication challenge.
      QTcpSocket excess;
      QSignalSpy disconnected(&excess, &QTcpSocket::disconnected);
      excess.connectToHost(QHostAddress::LocalHost, this->server.serverPort());
      QTRY_COMPARE(disconnected.size(), 1);
      QCOMPARE(this->manager->connections.size(), 1);
      bool found = false;
      for (auto* timer : this->manager->connections[0]->findChildren<QTimer*>())
         if (timer->interval() == 10000)
         {
            timer->start(20);
            found = true;
         }
      QVERIFY(found);
      QTRY_VERIFY(this->manager->connections.isEmpty());
      QTRY_COMPARE(stalled.state(), QAbstractSocket::UnconnectedState);
   }

   void cancelDuringTlsHandshake()
   {
      // Leave the accepted TCP socket idle, before the server starts TLS.
      TestConnection client(this->controller);
      QSignalSpy errors(&client, &RCC::InternalCoreConnection::connectingError);
      QSignalSpy connected(&client, &RCC::InternalCoreConnection::connected);
      this->connectClient(client);
      QTRY_VERIFY(this->server.hasPendingConnections());
      QScopedPointer<QTcpSocket> idle(this->server.nextPendingConnection());
      QTRY_VERIFY(idle->bytesAvailable() > 0); // ClientHello has been sent.
      client.disconnectFromCore();
      QTRY_COMPARE(client.transport()->state(), QAbstractSocket::UnconnectedState);
      QVERIFY(!client.connectionTimeoutTimer.isActive());
      QVERIFY(!client.retryTimer.isActive());
      QCOMPARE(errors.size(), 0);
      QCOMPARE(connected.size(), 0);
   }

   void remotePlaintextRejected()
   {
      this->startManager();
      QTcpSocket plaintext;
      QSignalSpy disconnected(&plaintext, &QTcpSocket::disconnected);
      plaintext.connectToHost(QHostAddress::LocalHost, this->server.serverPort());
      QTRY_COMPARE(this->manager->connections.size(), 1);
      plaintext.write("This is not a TLS ClientHello");
      QTRY_COMPARE(disconnected.size(), 1);
      // OpenSSL may answer with a fatal TLS alert record; anything else would be a plaintext leak.
      const QByteArray reply = plaintext.readAll();
      QVERIFY2(reply.isEmpty() || (reply.size() == 7 && reply.startsWith("\x15\x03")), reply.toHex().constData());
      QTRY_VERIFY(this->manager->connections.isEmpty());
   }
};

int main(int argc, char** argv)
{
   // Keep the log directory alive until Qt's post routines close the logger.
   // Each test uses a separate roaming directory for keys and certificate pins.
   QTemporaryDir logs;
   int result;
   {
      QCoreApplication application(argc, argv);
      Common::Global::setDataFolder(Common::Global::DataFolderType::LOCAL, logs.path());
      Tests tests;
      result = QTest::qExec(&tests, argc, argv);
   }
   return result;
}
#include "TlsTests.moc"
