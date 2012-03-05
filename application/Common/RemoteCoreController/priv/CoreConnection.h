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

namespace RCC
{
   class BrowseResult;
   class SearchResult;

   class CoreConnection : public ICoreConnection
   {
      Q_OBJECT

      static const int RETRY_PERIOD = 1000; // [ms]. When a connection failed, we automatically retry each 1s.

   protected:
      class Logger : public ILogger
      {
      public:
         void logDebug(const QString& message);
         void logError(const QString& message);
      };

   public:
      CoreConnection();
      ~CoreConnection();

      void connectToCore();
      void connectToCore(quint16 port);
      void connectToCore(const QString& address, quint16 port, Common::Hash password);

      bool isConnected() const;

      void disconnectFromCore();

      Common::Hash getOurID() const;
      void sendChatMessage(const QString& message);
      void setCoreSettings(const Protos::GUI::CoreSettings settings);
      void setCoreLanguage(const QLocale locale);

      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID);
      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entry& entry);
      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots = true);

      QSharedPointer<ISearchResult> search(const QString& terms);

      void download(const Common::Hash& peerID, const Protos::Common::Entry& entry);
      void download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const Common::Hash& sharedFolderID, const QString& path = "/");
      void download(const Common::Hash& peerID, const Protos::Common::Entry& entry, const QString& absolutePath);
      void cancelDownloads(const QList<quint64>& downloadIDs, bool complete = false);
      void moveDownloads(quint64 downloadIDRef, const QList<quint64>& downloadIDs, bool moveBefore = true);
      void refresh();

      bool isRunningAsSubProcess();

   signals:
      void browseResult(const Protos::GUI::BrowseResult& browseResult);
      void searchResult(const Protos::Common::FindResult& findResult);

   private slots:
      void connectToCoreSlot();
      void stateChanged(QAbstractSocket::SocketState socketState);
      void adressResolved(QHostInfo hostInfo);
      void connected();

   private:      
      void connectedAndAuthenticated();

      void onNewMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message);
      void onDisconnected();

      void sendCurrentLanguage();

      friend class BrowseResult;
      friend class SearchResult;

      void tryToConnectToTheNextAddress();

      CoreStatus coreStatus;

      QString currentAddress;
      quint16 currentPort;
      Common::Hash currentPassword;
      QLocale currentLanguage;

      QElapsedTimer timerFromLastConnectionTry;
      QTimer retryTimer;

      int currentHostLookupID;

      QList<QHostAddress> addressesToTry; // When a name is resolved many addresses can be returned, we will try all of them until a connection is successfuly established.

      QList< QWeakPointer<BrowseResult> > browseResultsWithoutTag;
      QList< QWeakPointer<SearchResult> > searchResultsWithoutTag;
      bool authenticated;
   };
}

#endif
