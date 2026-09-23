/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#include <priv/CoreConnection.h>
using namespace RCC;

#include <QHostAddress>
#include <QCoreApplication>
#include <QRandomGenerator64>
#include <QSslSocket>
#include <QSslError>

#include <Common/ProtoHelper.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/Network/RemoteControlTls.h>

#include <LogManager/Builder.h>

#include <priv/Log.h>
#include <priv/SendChatMessageResult.h>
#include <priv/BrowseResult.h>
#include <priv/LocalBrowseResult.h>
#include <priv/LocalBrowseQuickAccessResult.h>
#include <priv/SearchResult.h>

// The behavior under Windows and Linux are not the same when connecting a socket to a port.
// On Linux 'connectToHost(..)' will immediately fail if there is no service behind the port,
// on Windows there is a delay before 'stateChanged' is called with a 'UnconnectedState' type.
#ifdef Q_OS_WIN32
   const int InternalCoreConnection::NB_RETRIES_MAX(1);
   const int InternalCoreConnection::TIME_BETWEEN_RETRIES(100);
#else
   const int InternalCoreConnection::NB_RETRIES_MAX(8);
   const int InternalCoreConnection::TIME_BETWEEN_RETRIES(250);
#endif

// 'connectToHost(..)' is asynchronous and Qt gives up only after 30 s, for instance when the
// packets are silently dropped by a firewall. Each attempt is aborted after this delay instead.
const int InternalCoreConnection::CONNECTION_TIMEOUT(3000);

void InternalCoreConnection::Logger::logDebug(const QString& message)
{
   L_DEBU(message);
}

void InternalCoreConnection::Logger::logError(const QString& message)
{
   L_WARN(message);
}

InternalCoreConnection::InternalCoreConnection(CoreController& coreController) :
   Common::MessageSocket(new InternalCoreConnection::Logger(), new QSslSocket()),
   coreController(coreController),
   currentHostLookupID(-1),
   nbRetries(0),
   authenticated(false),
   forcedToClose(false),
   salt(0)
{
   this->retryTimer.setSingleShot(true);
   this->retryTimer.setInterval(TIME_BETWEEN_RETRIES);
   connect(&this->retryTimer, &QTimer::timeout, this, &InternalCoreConnection::tryToConnectToTheNextAddress);

   this->connectionTimeoutTimer.setSingleShot(true);
   this->connectionTimeoutTimer.setInterval(CONNECTION_TIMEOUT);
   connect(&this->connectionTimeoutTimer, &QTimer::timeout, this, &InternalCoreConnection::connectionTimedOut);
   auto* ssl = static_cast<QSslSocket*>(this->socket);
   connect(ssl, &QSslSocket::connected, this, [this] {
      if (!this->tlsRequired)
         this->startListening();
   });
   connect(ssl, &QSslSocket::sslErrors, this, [this, ssl](const QList<QSslError>& errors) {
      if (!this->tlsRequired || this->tlsFailureReported)
         return;
      try
      {
         Common::RemoteControlTls::checkPeer(this->connectionInfo.address, this->connectionInfo.port, ssl->peerCertificate());
         // A per-endpoint certificate pin replaces CA/hostname trust. All
         // other verification errors (expiry, bad signatures, etc.) are fatal.
         for (const auto& error : errors)
            if (error.error() != QSslError::SelfSignedCertificate &&
                error.error() != QSslError::CertificateUntrusted &&
                error.error() != QSslError::HostNameMismatch)
               throw error.errorString();
         ssl->ignoreSslErrors(errors);
      }
      catch (const QString& error)
      {
         this->tlsFailed(error);
      }
   });
   connect(ssl, &QSslSocket::encrypted, this, [this, ssl] {
      if (!this->tlsRequired || this->tlsFailureReported)
         return;
      try
      {
         // Also enforce pins when the presented certificate has a valid CA
         // chain and therefore does not produce sslErrors().
         Common::RemoteControlTls::checkPeer(this->connectionInfo.address, this->connectionInfo.port, ssl->peerCertificate());
         this->connectionTimeoutTimer.start();
         this->startListening();
      }
      catch (const QString& error)
      {
         this->tlsFailed(error);
      }
   });
   connect(ssl, &QSslSocket::errorOccurred, this, [this, ssl](QAbstractSocket::SocketError error) {
      if (this->tlsRequired && !this->tlsFailureReported &&
          (error == QAbstractSocket::SslHandshakeFailedError || error == QAbstractSocket::SslInternalError ||
           error == QAbstractSocket::SslInvalidUserDataError))
         this->tlsFailed(ssl->errorString());
   });
}

InternalCoreConnection::~InternalCoreConnection()
{
   this->cancelConnectionAttempt();
}

void InternalCoreConnection::cancelConnectionAttempt()
{
   ++this->attemptGeneration;
   if (this->currentHostLookupID != -1)
   {
      QHostInfo::abortHostLookup(this->currentHostLookupID);
      this->currentHostLookupID = -1;
   }
   this->retryTimer.stop();
   this->connectionTimeoutTimer.stop();
   this->connectionAttemptActive = false;
   // Closing a connecting socket must not schedule another address or retry.
   disconnect(this->socket, &QAbstractSocket::stateChanged, this, &InternalCoreConnection::stateChanged);
   this->addressesToTry.clear();
   this->addressesToRetry.clear();
   this->nbRetries = 0;
}

void InternalCoreConnection::connectToCore(const QString& address, quint16 port, Common::Hash password)
{
   this->startConnection(address, port, password, QString());
}

void InternalCoreConnection::connectToCore(const QString& address, quint16 port, const QString& password)
{
   this->startConnection(address, port, Common::Hash(), password);
}

/**
  * Each attempt sets both passwords: a plain one left by a failed attempt would otherwise be salted
  * and used in place of the hash given to the next one.
  */
void InternalCoreConnection::startConnection(const QString& address, quint16 port, const Common::Hash& password, const QString& plainPassword)
{
   this->cancelConnectionAttempt();

   this->connectionInfo.address = address;
   this->connectionInfo.port = port;
   this->connectionInfo.password = password;
   this->password = plainPassword;

   this->currentHostLookupID =
      QHostInfo::lookupHost(this->connectionInfo.address, this, &InternalCoreConnection::addressResolved);
}

bool InternalCoreConnection::isLocal() const
{
   if (this->socket->peerAddress().isNull())
      return Common::Global::isLocal(QHostAddress(this->connectionInfo.address));
   else
      return MessageSocket::isLocal();
}

bool InternalCoreConnection::isConnected() const
{
   return MessageSocket::isConnected() && this->authenticated;
}

void InternalCoreConnection::disconnectFromCore()
{
   this->cancelConnectionAttempt();
   this->connectionInfo.clear();
   this->password.clear();
   this->forcedToClose = true;
   // close() can defer shutdown until an in-progress TCP connection completes.
   if (this->socket->state() == QAbstractSocket::HostLookupState || this->socket->state() == QAbstractSocket::ConnectingState)
      this->socket->abort();
   else
      this->close();
   // Pending writes can delay onDisconnected(), which still needs the reason.
   // An idle or aborted socket must not retain the flag for the next connection.
   if (this->socket->state() == QAbstractSocket::UnconnectedState)
      this->forcedToClose = false;
}

QSharedPointer<ISendChatMessageResult> InternalCoreConnection::sendChatMessage(
   int socketTimeout,
   const QString& message,
   const QString& roomName,
   const QList<Common::Hash>& peerIDsAnswered
)
{
   return QSharedPointer<SendChatMessageResult>::create(this, socketTimeout, message, roomName, peerIDsAnswered);
}

void InternalCoreConnection::joinRoom(const QString& room)
{
   if (!room.isEmpty())
   {
      Protos::GUI::JoinRoom joinRoomMessage;
      joinRoomMessage.set_name(room.toStdString());
      this->send(Common::MessageHeader::GUI_JOIN_ROOM, joinRoomMessage);
   }
}

void InternalCoreConnection::leaveRoom(const QString& room)
{
   if (!room.isEmpty())
   {
      Protos::GUI::LeaveRoom leaveRoomMessage;
      leaveRoomMessage.set_name(room.toStdString());
      this->send(Common::MessageHeader::GUI_LEAVE_ROOM, leaveRoomMessage);
   }
}

void InternalCoreConnection::setCoreSettings(const Protos::GUI::CoreSettings& settings)
{
   this->send(Common::MessageHeader::GUI_SETTINGS, settings);
}

void InternalCoreConnection::setCoreLanguage(const QLocale& locale)
{
   this->currentLanguage = locale;
   this->sendCurrentLanguage();
}

bool InternalCoreConnection::setCorePassword(const QString& newPassword, const QString& oldPassword)
{
   Protos::GUI::ChangePassword passMess;

   const quint64 newSalt = QRandomGenerator64::global()->generate64();
   Common::Hash newPasswordHashed = Common::Hasher::hashWithSalt(newPassword, newSalt);

   passMess.mutable_new_password()->set_hash(newPasswordHashed.getData(), Common::Hash::HASH_SIZE);
   passMess.set_new_salt(newSalt);

   if (!oldPassword.isNull())
   {
      Common::Hash oldPasswordHashed = Common::Hasher::hashWithSalt(oldPassword, this->salt);
      if (!this->connectionInfo.password.isNull() && this->connectionInfo.password != oldPasswordHashed)
         return false;

      passMess.mutable_old_password()->set_hash(oldPasswordHashed.getData(), Common::Hash::HASH_SIZE);
   }

   this->connectionInfo.password = newPasswordHashed;
   this->salt = newSalt;

   this->send(Common::MessageHeader::GUI_CHANGE_PASSWORD, passMess);
   return true;
}

void InternalCoreConnection::resetCorePassword()
{
   Protos::GUI::ChangePassword passMess;
   passMess.mutable_new_password()->set_hash(Common::Hash().getData(), Common::Hash::HASH_SIZE);
   passMess.set_new_salt(0);
   this->send(Common::MessageHeader::GUI_CHANGE_PASSWORD, passMess);
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID, int socketTimeout)
{
   return QSharedPointer<BrowseResult>::create(this, peerID, socketTimeout);
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entry& entry, int socketTimeout)
{
   return QSharedPointer<BrowseResult>::create(this, peerID, entry, socketTimeout);
}

QSharedPointer<IBrowseResult> InternalCoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots, int socketTimeout)
{
   return QSharedPointer<BrowseResult>::create(this, peerID, entries, withRoots, socketTimeout);
}

QSharedPointer<ILocalBrowseResult> InternalCoreConnection::localBrowse(const QString& path, bool onlyDirectories, int socketTimeout)
{
   return QSharedPointer<LocalBrowseResult>::create(this, path, onlyDirectories, socketTimeout);
}

QSharedPointer<ILocalBrowseQuickAccessResult> InternalCoreConnection::localBrowseQuickAccess(int socketTimeout)
{
   return QSharedPointer<LocalBrowseQuickAccessResult>::create(this, socketTimeout);
}

QSharedPointer<ISearchResult> InternalCoreConnection::search(const Protos::Common::FindPattern& findPattern, bool local, int socketTimeout)
{
   return QSharedPointer<SearchResult>::create(this, findPattern, local, socketTimeout);
}

void InternalCoreConnection::download(
   const Common::Hash& peerID,
   const Protos::Common::Entry& entry,
   const Common::Hash& sharedFolderID,
   const Common::Path& path
)
{
   // We cannot download our entries.
   if (peerID == this->getLocalID())
      return;

   Protos::GUI::Download downloadMessage;
   downloadMessage.mutable_peer_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
   downloadMessage.mutable_entry()->CopyFrom(entry);
   if (!sharedFolderID.isNull())
      downloadMessage.mutable_destination_directory_id()->set_hash(sharedFolderID.getData(), Common::Hash::HASH_SIZE);
   downloadMessage.set_destination_path(path.toString().toStdString());
   this->send(Common::MessageHeader::GUI_DOWNLOAD, downloadMessage);
}

void InternalCoreConnection::cancelDownloads(const QList<quint64>& downloadIDs, bool complete)
{
   Protos::GUI::CancelDownloads cancelDownloadsMessage;
   for (const quint64 id : downloadIDs)
      cancelDownloadsMessage.add_ids(id);
   cancelDownloadsMessage.set_complete(complete);
   this->send(Common::MessageHeader::GUI_CANCEL_DOWNLOADS, cancelDownloadsMessage);
}

void InternalCoreConnection::pauseDownloads(const QList<quint64>& downloadIDs, bool pause)
{
   Protos::GUI::PauseDownloads pauseDownloadsMessage;
   for (const quint64 id : downloadIDs)
      pauseDownloadsMessage.add_ids(id);
   pauseDownloadsMessage.set_pause(pause);
   this->send(Common::MessageHeader::GUI_PAUSE_DOWNLOADS, pauseDownloadsMessage);
}

void InternalCoreConnection::moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position)
{
   if (downloadIDRefs.isEmpty() || downloadIDs.isEmpty()) // Nothing to do in this case.
      return;

   Protos::GUI::MoveDownloads moveDownloadsMessage;
   for (const quint64 id : downloadIDRefs)
      moveDownloadsMessage.add_ids_ref(id);
   moveDownloadsMessage.set_position(position);
   for (const quint64 id : downloadIDs)
      moveDownloadsMessage.add_ids_to_move(id);
   this->send(Common::MessageHeader::GUI_MOVE_DOWNLOADS, moveDownloadsMessage);
}

void InternalCoreConnection::refresh()
{
   this->send(Common::MessageHeader::GUI_REFRESH);
}

void InternalCoreConnection::refreshNetworkInterfaces()
{
   this->send(Common::MessageHeader::GUI_REFRESH_NETWORK_INTERFACES);
}

ICoreConnection::ConnectionInfo InternalCoreConnection::getConnectionInfo() const
{
   return this->connectionInfo;
}

void InternalCoreConnection::addressResolved(QHostInfo hostInfo)
{
   // A result queued before cancellation may belong to an earlier attempt.
   if (this->currentHostLookupID == -1 || hostInfo.lookupId() != this->currentHostLookupID)
      return;
   this->currentHostLookupID = -1;

   if (hostInfo.addresses().isEmpty())
   {
      emit connectingError(ICoreConnection::RCC_ERROR_HOST_UNKOWN);
      return;
   }

   this->addressesToTry = hostInfo.addresses();
   this->addressesToRetry.clear();
   this->nbRetries = 0;
   this->tryToConnectToTheNextAddress();
}

void InternalCoreConnection::tryToConnectToTheNextAddress()
{
   if (this->addressesToTry.isEmpty())
      return;

   const quint64 generation = this->attemptGeneration;

   // Try the IPv6 addresses first.
   const auto ipv6 = std::find_if(this->addressesToTry.cbegin(), this->addressesToTry.cend(), [](const QHostAddress& address) {
      return address.protocol() == QAbstractSocket::IPv6Protocol;
   });
   const QHostAddress address =
      this->addressesToTry.takeAt(ipv6 == this->addressesToTry.cend() ? 0 : ipv6 - this->addressesToTry.cbegin());

   L_DEBU(QString("Trying to connect to %1 (nb retry: %2) ...").arg(address.toString()).arg(this->nbRetries));

   // The core is launched manually in debug mode.
#if !defined(DEBUG)
   // If the address is local then check if the core is launched, if not try to launch it.
   if (Common::Global::isLocal(address))
   {
      this->coreController.startCore(this->connectionInfo.port);
      L_DEBU(QString("Core controller status: %1").arg(this->coreController.getStatus()));
   }
#endif

   // Starting the local core can emit signals whose handlers cancel this attempt.
   if (generation != this->attemptGeneration)
      return;

   connect(this->socket, &QAbstractSocket::stateChanged, this, &InternalCoreConnection::stateChanged);
   this->addressesToRetry << address;
   this->connectionAttemptActive = true;
   // Arm before connectToHost: a synchronous failure/success must be able to
   // stop the timeout without it being restarted after the callback returns.
   this->connectionTimeoutTimer.start();
   this->stopListening();
   this->tlsRequired = !Common::Global::isLocal(address);
   this->tlsFailureReported = false;
   auto* ssl = static_cast<QSslSocket*>(this->socket);
   // Reset any exceptions remembered by QSslSocket from an earlier handshake.
   ssl->ignoreSslErrors(QList<QSslError>());
   if (this->tlsRequired)
   {
      if (!QSslSocket::supportsSsl())
      {
         this->tlsFailed("No Qt TLS backend is available");
         return;
      }
      QSslConfiguration configuration = QSslConfiguration::defaultConfiguration();
      configuration.setProtocol(QSsl::TlsV1_2OrLater);
      configuration.setPeerVerifyMode(QSslSocket::VerifyPeer);
      ssl->setSslConfiguration(configuration);
      ssl->connectToHostEncrypted(address.toString(), this->connectionInfo.port, this->connectionInfo.address);
   }
   else
      ssl->connectToHost(address, this->connectionInfo.port);
}

void InternalCoreConnection::tlsFailed(const QString& reason)
{
   if (this->tlsFailureReported)
      return;
   this->tlsFailureReported = true;
   const bool wasAuthenticated = this->authenticated;
   L_WARN(QString("Remote-control TLS connection refused: %1").arg(reason));
   this->cancelConnectionAttempt();
   // Abort without reporting a second, misleading timeout via disconnected().
   this->connectionAttemptActive = true;
   this->stopListening();
   this->socket->abort();
   this->connectionAttemptActive = false;
   if (wasAuthenticated)
      emit disconnected(false);
   else
      emit connectingError(ICoreConnection::RCC_ERROR_TLS);
}

void InternalCoreConnection::connectionTimedOut()
{
   L_DEBU(QString("Connection/authentication with %1:%2 timed out after %3 ms")
      .arg(this->connectionInfo.address).arg(this->connectionInfo.port).arg(this->connectionTimeoutTimer.interval()));

   // 'abort()' puts the socket in 'UnconnectedState', 'stateChanged(..)' then tries the next address or retries.
   this->socket->abort();
}

void InternalCoreConnection::stateChanged(QAbstractSocket::SocketState socketState)
{
   switch (socketState)
   {
   case QAbstractSocket::UnconnectedState:
      disconnect(this->socket, &QAbstractSocket::stateChanged, this, &InternalCoreConnection::stateChanged);
      this->connectionTimeoutTimer.stop();
      if (!this->addressesToTry.isEmpty())
      {
         // Finish the old socket's notifications before starting another address.
         this->retryTimer.start(0);
      }
      else if (this->nbRetries++ < NB_RETRIES_MAX)
      {
         this->addressesToTry = this->addressesToRetry;
         this->addressesToRetry.clear();
         this->retryTimer.start(TIME_BETWEEN_RETRIES);
      }
      else
      {
         this->connectionAttemptActive = false;
         emit connectingError(ICoreConnection::RCC_ERROR_HOST_TIMEOUT);
      }
      break;

   case QAbstractSocket::ConnectedState:
      L_DEBU("Core TCP connection opened; waiting for authentication");
      // TCP alone is not success: macOS can report ConnectedState immediately
      // before closing an unavailable endpoint. Keep retries and the timeout
      // active until the core completes authentication.
      this->connectionTimeoutTimer.start();
      break;

   default:;
   }
}

void InternalCoreConnection::connectedAndAuthenticated()
{
   if (this->tlsRequired)
   {
      auto* ssl = static_cast<QSslSocket*>(this->socket);
      if (!ssl->isEncrypted())
      {
         this->tlsFailed("The remote connection is not encrypted");
         return;
      }
      try
      {
         Common::RemoteControlTls::rememberPeer(this->connectionInfo.address, this->connectionInfo.port, ssl->peerCertificate());
         L_DEBU(QString("Trusted Core TLS certificate SHA-256: %1")
            .arg(Common::RemoteControlTls::fingerprint(ssl->peerCertificate())));
      }
      catch (const QString& error)
      {
         this->tlsFailed(error);
         return;
      }
   }
   this->cancelConnectionAttempt();
   // If we were previously connected we announce it.
   if (this->authenticated)
      emit disconnected(this->forcedToClose);

   this->authenticated = true;

   this->sendCurrentLanguage();
   emit connected();
}

void InternalCoreConnection::sendCurrentLanguage()
{
   if (this->authenticated)
   {
      Protos::GUI::Language langMess;
      Common::ProtoHelper::setLang(*langMess.mutable_language(), this->currentLanguage);
      this->send(Common::MessageHeader::GUI_LANGUAGE, langMess);
   }
}

void InternalCoreConnection::onNewMessage(const Common::Message& message)
{
   // While we are not authenticated we accept only two message types.
   if (
      !this->authenticated && message.getHeader().getType() != Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION &&
      message.getHeader().getType() != Common::MessageHeader::GUI_AUTHENTICATION_RESULT
   )
      return;

   switch (message.getHeader().getType())
   {
   case Common::MessageHeader::GUI_ASK_FOR_AUTHENTICATION:
      {
         const Protos::GUI::AskForAuthentication& askForAuthentication = message.getMessage<Protos::GUI::AskForAuthentication>();

         Protos::GUI::Authentication authentication;

         this->salt = askForAuthentication.salt();

         if (!this->password.isEmpty())
            this->connectionInfo.password = Common::Hasher::hashWithSalt(this->password, this->salt);

         authentication.mutable_password_challenge()->set_hash(Common::Hasher::hashWithSalt(this->connectionInfo.password, askForAuthentication.salt_challenge()).getData(), Common::Hash::HASH_SIZE);
         this->password.clear();
         this->send(Common::MessageHeader::GUI_AUTHENTICATION, authentication);
      }
      break;

   case Common::MessageHeader::GUI_AUTHENTICATION_RESULT:
      {
         const Protos::GUI::AuthenticationResult& authenticationResult = message.getMessage<Protos::GUI::AuthenticationResult>();

         if (authenticationResult.status() == Protos::GUI::AuthenticationResult::AUTH_OK)
         {
            this->connectedAndAuthenticated();
         }
         else
         {
            // An explicit authentication refusal is final, unlike a transient
            // disconnect before the handshake finishes.
            this->cancelConnectionAttempt();
            switch (authenticationResult.status())
            {
            case Protos::GUI::AuthenticationResult::AUTH_PASSWORD_NOT_DEFINED:
               emit connectingError(ICoreConnection::RCC_ERROR_NO_REMOTE_PASSWORD_DEFINED);
               break;

            case Protos::GUI::AuthenticationResult::AUTH_BAD_PASSWORD:
                emit connectingError(ICoreConnection::RCC_ERROR_WRONG_PASSWORD);
               break;

            case Protos::GUI::AuthenticationResult::AUTH_ERROR:
            default:
               emit connectingError(ICoreConnection::RCC_ERROR_UNKNOWN);
               break;
            }
         }
      }
      break;

   case Common::MessageHeader::GUI_STATE:
      {
         const Protos::GUI::State& state = message.getMessage<Protos::GUI::State>();

         emit newState(state);
         this->send(Common::MessageHeader::GUI_STATE_RESULT);
      }
      break;

   case Common::MessageHeader::GUI_EVENT_CHAT_MESSAGES:
      {
         const Protos::Common::ChatMessages& chatMessages = message.getMessage<Protos::Common::ChatMessages>();
         if (chatMessages.messages_size() > 0)
            emit newChatMessages(chatMessages);
      }
      break;

   case Common::MessageHeader::GUI_EVENT_LOG_MESSAGES:
      {
         const Protos::GUI::EventLogMessages& eventLogMessages = message.getMessage<Protos::GUI::EventLogMessages>();

         QList<QSharedPointer<LM::IEntry>> entries;
         entries.reserve(eventLogMessages.messages_size());
         for (const auto& logMessage : eventLogMessages.messages())
            entries << LM::Builder::newEntry(
               QDateTime::fromMSecsSinceEpoch(logMessage.time()),
               LM::Severity(logMessage.severity()),
               QString::fromStdString(logMessage.message())
            );

         emit newLogMessages(entries);
      }
      break;

   // Each reply consumes exactly one sent request. An expired weak pointer is a
   // placeholder for a discarded request, not permission to use the next one.
   case Common::MessageHeader::GUI_CHAT_MESSAGE_RESULT:
      if (!this->sendChatMessageResultWithoutReply.isEmpty())
      {
         if (auto result = this->sendChatMessageResultWithoutReply.takeFirst().toStrongRef())
            result->setResult(message.getMessage<Protos::GUI::ChatMessageResult>());
      }
      break;

   case Common::MessageHeader::GUI_SEARCH_RESULT:
      {
         const Protos::Common::FindResult& findResultMessage = message.getMessage<Protos::Common::FindResult>();
         emit searchResult(findResultMessage);
      }
      break;

   case Common::MessageHeader::GUI_BROWSE_RESULT:
      {
         const Protos::GUI::BrowseResult& browseResultMessage = message.getMessage<Protos::GUI::BrowseResult>();
         emit browseResult(browseResultMessage);
      }
      break;

   case Common::MessageHeader::GUI_LOCAL_BROWSE_RESULT:
      {
         const Protos::GUI::LocalBrowseResult& browseResultMessage = message.getMessage<Protos::GUI::LocalBrowseResult>();
         emit localBrowseResult(browseResultMessage);
      }
      break;

   case Common::MessageHeader::GUI_LOCAL_BROWSE_QUICK_ACCESS_RESULT:
      {
         const Protos::GUI::LocalBrowseQuickAccessResult& browseResultMessage =
            message.getMessage<Protos::GUI::LocalBrowseQuickAccessResult>();
         emit localBrowseQuickAccessResult(browseResultMessage);
      }
      break;

   default:;
   }
}

void InternalCoreConnection::onDisconnected()
{
   this->authenticated = false;
   this->sendChatMessageResultWithoutReply.clear();
   const bool asked = this->forcedToClose;
   this->forcedToClose = false;
   // A connection is established only after authentication. Until then,
   // stateChanged owns retries and the final timeout notification.
   if (!this->connectionAttemptActive)
      emit disconnected(asked);
}
