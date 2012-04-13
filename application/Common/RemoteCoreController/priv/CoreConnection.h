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
  
#ifndef RCC_CORECONNECTION_H
#define RCC_CORECONNECTION_H

#include <QObject>
#include <QTcpSocket>
#include <QHostInfo>
#include <QSharedPointer>
#include <QWeakPointer>
#include <QProcess>
#include <QElapsedTimer>
#include <QLocale>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/Timeoutable.h>
#include <Common/LogManager/IEntry.h>

#include <ICoreConnection.h>
#include <IBrowseResult.h>
#include <ISearchResult.h>
#include <Types.h>
#include <priv/InternalCoreConnection.h>

namespace RCC
{
   class BrowseResult;
   class SearchResult;

   class CoreConnection : public ICoreConnection
   {
      Q_OBJECT
      static const int DEFAULT_SOCKET_TIMEOUT = 6000; // 6 seconds.

   public:
      CoreConnection(int socketTimeout = DEFAULT_SOCKET_TIMEOUT);
      ~CoreConnection();

      void connectToCore();
      void connectToCore(quint16 port);
      void connectToCore(const QString& address, quint16 port, Common::Hash password);
      void connectToCore(const QString& address, quint16 port, const QString& password);

      Common::Hash getLocalID() const;
      Common::Hash getRemoteID() const;

      bool isLocal() const;
      bool isConnected() const;
      bool isConnecting() const;

      void disconnectFromCore();

      void sendChatMessage(const QString& message);
      void setCoreSettings(const Protos::GUI::CoreSettings settings);
      void setCoreLanguage(const QLocale locale);
      bool setCorePassword(const QString& newPassword, const QString& oldPassword = QString());
      void resetCorePassword();

      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID);
      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entry& entry);
      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots = true);

      QSharedPointer<ISearchResult> search(const QString& terms);

      void download(const Common::Hash& peerID, const Protos::Common::Entry& entry);
      void download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const Common::Hash& sharedFolderID, const QString& path = "/");
      void download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const QString& absolutePath);
      void cancelDownloads(const QList<quint64>& downloadIDs, bool complete = false);
      void pauseDownloads(const QList<quint64>& downloadIDs, bool pause = true);
      void moveDownloads(quint64 downloadIDRef, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position = Protos::GUI::MoveDownloads::BEFORE);
      void moveDownloads(const QList<quint64>& downloadIDRefs, const QList<quint64>& downloadIDs, Protos::GUI::MoveDownloads::Position position = Protos::GUI::MoveDownloads::BEFORE);

      void refresh();

      bool isRunningAsSubProcess() const;

      ConnectionInfo getConnectionInfo() const;
      ConnectionInfo getConnectionInfoConnecting() const;

   private slots:
      void tempConnectingError(RCC::ICoreConnection::ConnectionErrorCode code);
      void tempConnected();
      void tempDisconnected();

   private:
      bool connectToCorePrepare(const QString& address);

      InternalCoreConnection* current();
      const InternalCoreConnection* current() const;

      InternalCoreConnection* temp();
      const InternalCoreConnection* temp() const;

      void swap();

      InternalCoreConnection connections[2];
      int currentConnected;

      bool connectingInProgress;

      const int SOCKET_TIMEOUT;
   };
}

#endif
