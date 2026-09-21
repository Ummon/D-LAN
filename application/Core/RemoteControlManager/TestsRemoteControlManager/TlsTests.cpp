#include <memory>

#include <QTest>
#include <QSignalSpy>
#include <QTemporaryDir>
#include <QFile>
#include <QDir>
#include <QSslKey>

#include <Common/Global.h>
#include <Common/Settings.h>
#include <Common/Network/RemoteControlTls.h>
#include <Common/RemoteCoreController/priv/InternalCoreConnection.h>
#include <Core/RemoteControlManager/priv/RemoteControlManager.h>
#include <Core/PeerManager/Builder.h>
#include <Protos/core_settings.pb.h>
#include "Mocks.h"

namespace Tls = Common::RemoteControlTls;

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

class Tests : public QObject
{
   Q_OBJECT
   std::unique_ptr<QTemporaryDir> directory;
   std::unique_ptr<RCM::RemoteControlManager> manager;
   TestServer server;
   RCC::CoreController controller;

   QString identityPath() const { return this->directory->filePath("remote-control-tls/core.pem"); }

   void startManager()
   {
      auto files = QSharedPointer<FileManager>::create();
      this->manager = std::make_unique<RCM::RemoteControlManager>(files, PM::Builder::newPeerManager(files),
         QSharedPointer<UploadManager>::create(), QSharedPointer<DownloadManager>::create(),
         QSharedPointer<NetworkListener>::create(), QSharedPointer<ChatSystem>::create());
      connect(&this->server, &QTcpServer::newConnection, this->manager.get(), &RCM::RemoteControlManager::newConnection);
   }

   void connectClient(TestConnection& client, bool remote = true, bool correctPassword = true)
   {
      client.connectionInfo.address = "test-core";
      client.connectionInfo.port = this->server.serverPort();
      client.connectionInfo.password = correctPassword ? SETTINGS.get<Common::Hash>("remote_password") : Common::Hash::rand();
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
         client.transport()->connectToHostEncrypted("127.0.0.1", this->server.serverPort(), "test-core");
      }
      else
         client.transport()->connectToHost(QHostAddress::LocalHost, this->server.serverPort());
   }

private slots:
   void initTestCase()
   {
      const QString backend = qEnvironmentVariable("DLAN_TEST_TLS_BACKEND");
      if (!backend.isEmpty())
         QVERIFY2(QSslSocket::setActiveBackend(backend), qPrintable(backend));
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
      SETTINGS.set("remote_password", Common::Hash::rand());
      SETTINGS.set("salt", quint64(123));
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
      QVERIFY(first.localCertificate().subjectInfo(QSslCertificate::CommonName).isEmpty());
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
      QVERIFY(plaintext.readAll().isEmpty());
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
