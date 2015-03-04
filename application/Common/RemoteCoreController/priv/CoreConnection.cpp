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

#include <LogManager/Builder.h>
#include <ProtoHelper.h>
#include <Constants.h>

#include <priv/CoreController.h>
#include <priv/Log.h>
#include <priv/BrowseResult.h>
#include <priv/SearchResult.h>

CoreConnection::CoreConnection(int socketTimeout) :
   connection1(this->coreController),
   connection2(this->coreController),
   currentConnected(FIRST_CONNECTION),
   connectingInProgress(false),
   SOCKET_TIMEOUT(socketTimeout)
{
   connect(&this->coreController, SIGNAL(statusChanged()), this, SIGNAL(localCoreStatusChanged()));
}

CoreConnection::~CoreConnection()
{
}

void CoreConnection::setCoreExecutableDirectory(const QString& dir)
{
   this->coreController.setCoreExecutableDirectory(dir);
}

void CoreConnection::startLocalCore()
{
   this->coreController.startCore();
}

void CoreConnection::stopLocalCore()
{
   this->coreController.stopCore();
}

CoreStatus CoreConnection::getLocalCoreStatus() const
{
   return this->coreController.getStatus();
}

void CoreConnection::connectToCore()
{
   this->connectToCore(Common::Constants::DEFAULT_CORE_REMOTE_CONTROL_PORT);
}

void CoreConnection::connectToCore(quint16 port)
{
   this->connectToCore("localhost", port, Common::Hash());
}

void CoreConnection::connectToCore(const QString& address, quint16 port, Common::Hash password)
{
   if (!this->connectToCorePrepare(address))
      return;
   this->temp().connectToCore(address, port, password);
}

void CoreConnection::connectToCore(const QString& address, quint16 port, const QString& password)
{
   if (!this->connectToCorePrepare(address))
      return;
   this->temp().connectToCore(address, port, password);
}

Common::Hash CoreConnection::getRemoteID() const
{
   return this->current().getRemoteID();
}

bool CoreConnection::isLocal() const
{
   if (this->connectingInProgress)
      return this->temp().isLocal();
   else
      return this->current().isLocal();
}

bool CoreConnection::isConnected() const
{
   return this->current().isConnected();
}

bool CoreConnection::isConnecting() const
{
   return this->connectingInProgress;
}

void CoreConnection::disconnectFromCore()
{
   this->current().disconnectFromCore();
   this->temp().disconnectFromCore();
}

QSharedPointer<ISendChatMessageResult> CoreConnection::sendChatMessage(const QString& message)
{
   return this->current().sendChatMessage(this->SOCKET_TIMEOUT, message);
}

QSharedPointer<ISendChatMessageResult> CoreConnection::sendChatMessage(const QString& message, const QString& roomName)
{
   return this->current().sendChatMessage(this->SOCKET_TIMEOUT, message, roomName);
}

QSharedPointer<ISendChatMessageResult> CoreConnection::sendChatMessage(const QString& message, const QString& roomName, const QList<Common::Hash>& peerIDsAnswered)
{
   return this->current().sendChatMessage(this->SOCKET_TIMEOUT, message, roomName, peerIDsAnswered);
}

void CoreConnection::joinRoom(const QString& room)
{
   this->current().joinRoom(room);
}

void CoreConnection::leaveRoom(const QString& room)
{
   this->current().leaveRoom(room);
}

void CoreConnection::setCoreSettings(const Protos::GUI::CoreSettings settings)
{
   this->current().setCoreSettings(settings);
}

void CoreConnection::setCoreLanguage(const QLocale& locale)
{
   this->current().setCoreLanguage(locale);
   this->temp().setCoreLanguage(locale);
}

bool CoreConnection::setCorePassword(const QString& newPassword, const QString& oldPassword)
{
   return this->current().setCorePassword(newPassword, oldPassword);
}

void CoreConnection::resetCorePassword()
{
   this->current().resetCorePassword();
}

QSharedPointer<IBrowseResult> CoreConnection::browse(const Common::Hash& peerID)
{
   return this->current().browse(peerID, this->SOCKET_TIMEOUT);
}

QSharedPointer<IBrowseResult> CoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entry& entry)
{

   return this->current().browse(peerID, entry, this->SOCKET_TIMEOUT);
}

QSharedPointer<IBrowseResult> CoreConnection::browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots)
{

   return this->current().browse(peerID, entries, withRoots, this->SOCKET_TIMEOUT);
}

QSharedPointer<ISearchResult> CoreConnection::search(const Protos::Common::FindPattern& findPattern, bool local)
{
   return this->current().search(findPattern, local, this->SOCKET_TIMEOUT);
}

void CoreConnection::download(const Common::Hash& peerID, const Protos::Common::Entry& entry)
{
   this->current().download(peerID, entry);
}

void CoreConnection::download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const Common::Hash& sharedFolderID, const QString& path)
{
   this->current().download(peerID, entry, sharedFolderID, path);
}

void CoreConnection::download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const QString& absolutePath)
{
   this->current().download(peerID, entry, Common::Hash(), absolutePath);
}

void CoreConnection::cancelDownloads(const QList<quint64>& downloadIDs, bool complete)
{
   this->current().cancelDownloads(downloadIDs, complete);
}

void CoreConnection::pauseDownloads(const QList<quint64>& downloadIDs, bool pause)
{
   this->current().pauseDownloads(downloadIDs, pause);
}

void CoreConnection::moveDownloads(quint64 downloadIDRef, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position)
{
   this->moveDownloads(QList<quint64>() << downloadIDRef, downloadIDs, position);
}

void CoreConnection::moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position)
{
   this->current().moveDownloads(downloadIDRefs, downloadIDs, position);
}

void CoreConnection::refresh()
{
   this->current().refresh();
}

void CoreConnection::refreshNetworkInterfaces()
{
   this->current().refreshNetworkInterfaces();
}

ICoreConnection::ConnectionInfo CoreConnection::getConnectionInfo() const
{
   return this->current().getConnectionInfo();
}

ICoreConnection::ConnectionInfo CoreConnection::getConnectionInfoConnecting() const
{
   return this->temp().getConnectionInfo();
}

void CoreConnection::tempConnectingError(RCC::ICoreConnection::ConnectionErrorCode code)
{
   this->connectingInProgress = false;
   emit connectingError(code);
   this->temp().disconnect(this);
}

void CoreConnection::tempConnected()
{
   this->current().disconnectFromCore(); // May throw a 'disconnected()' signal.

   this->current().disconnect(this);
   this->temp().disconnect(this);

   this->connectingInProgress = false;

   this->swap();

   connect(&this->current(), SIGNAL(disconnected(bool)), this, SIGNAL(disconnected(bool)));
   connect(&this->current(), SIGNAL(newState(const Protos::GUI::State&)), this, SIGNAL(newState(const Protos::GUI::State&)));
   connect(&this->current(), SIGNAL(newChatMessages(const Protos::Common::ChatMessages&)), this, SIGNAL(newChatMessages(const Protos::Common::ChatMessages&)));
   connect(&this->current(), SIGNAL(newLogMessages(QList<QSharedPointer<LM::IEntry>>)), this, SIGNAL(newLogMessages(QList<QSharedPointer<LM::IEntry>>)));
   emit connected();
}

void CoreConnection::tempDisconnected()
{
   if (this->connectingInProgress)
      this->tempConnectingError(RCC_ERROR_HOST_TIMEOUT);
}

/**
  * @return true is no error.
  */
bool CoreConnection::connectToCorePrepare(const QString& address)
{
   emit connecting();

   if (this->connectingInProgress)
   {
      this->tempConnectingError(RCC_ERROR_CONNECTING_IN_PROGRESS);
      return false;
   }

   this->connectingInProgress = true;

   if (address.isNull() || address.isEmpty())
   {
      this->tempConnectingError(RCC_ERROR_INVALID_ADDRESS);
      return false;
   }

   connect(&this->temp(), SIGNAL(connectingError(RCC::ICoreConnection::ConnectionErrorCode)), this, SLOT(tempConnectingError(RCC::ICoreConnection::ConnectionErrorCode)));
   connect(&this->temp(), SIGNAL(connected()), this, SLOT(tempConnected()));
   connect(&this->temp(), SIGNAL(disconnected(bool)), this, SLOT(tempDisconnected()));

   return true;
}

InternalCoreConnection& CoreConnection::current()
{
   return this->currentConnected == FIRST_CONNECTION ? this->connection1 : this->connection2;
}

const InternalCoreConnection& CoreConnection::current() const
{
   return this->currentConnected == FIRST_CONNECTION ? this->connection1 : this->connection2;
}

InternalCoreConnection& CoreConnection::temp()
{
   return this->currentConnected == FIRST_CONNECTION ? this->connection2 : this->connection1;
}

const InternalCoreConnection& CoreConnection::temp() const
{
   return this->currentConnected == FIRST_CONNECTION ? this->connection2 : this->connection1;
}

void CoreConnection::swap()
{
   this->currentConnected = this->currentConnected == FIRST_CONNECTION ? SECOND_CONNECTION : FIRST_CONNECTION;
}
