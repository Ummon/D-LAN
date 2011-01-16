/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
#include <QProcess>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/Timeoutable.h>
#include <Common/Network.h>
#include <Common/LogManager/IEntry.h>

#include <ICoreConnection.h>
#include <IBrowseResult.h>
#include <ISearchResult.h>

namespace RCC
{
   class BrowseResult;
   class SearchResult;
   class CoreConnection : public ICoreConnection
   {
      Q_OBJECT
   public:
      CoreConnection();
      ~CoreConnection();

      void connectToCore();
      void connectToCore(const QString& address);
      void connectToCore(const QString& address, quint16 port);
      void connectToCore(const QString& address, quint16 port, Common::Hash password);

      Common::Hash getOurID() const;
      void sendChatMessage(const QString& message);
      void setCoreSettings(const Protos::GUI::CoreSettings settings);

      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID);
      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entry& entry);
      QSharedPointer<IBrowseResult> browse(const Common::Hash& peerID, const Protos::Common::Entries& entries, bool withRoots = true);

      QSharedPointer<ISearchResult> search(const QString& terms);

      void download(const Common::Hash& peerID, const Protos::Common::Entry& entry);
      void cancelDownloads(const QList<quint64>& downloadIDs);
      void moveDownloads(quint64 downloadIDRef, const QList<quint64>& downloadIDs, bool moveBefore = true);
      void refresh();

      bool isConnected();
      bool isLocal();

   private slots:
      void connectToCoreSlot();
      void stateChanged(QAbstractSocket::SocketState socketState);
      void dataReceived();
      void adressResolved(QHostInfo hostInfo);
      void connected();
      void disconnected();

   private:
      friend class BrowseResult;
      friend class SearchResult;

      void tryToConnectToTheNextAddress();

      void send(Common::Network::GUIMessageType type);
      void send(Common::Network::GUIMessageType type, const google::protobuf::Message& message);
      bool readMessage();

      QString currentAddress;
      quint16 currentPort;
      Common::Hash currentPassword;

      QTcpSocket socket;
      Common::Hash ourID;
      Common::Network::MessageHeader<Common::Network::GUIMessageType> currentHeader;

      int currentHostLookupID;

      QList<QHostAddress> addressesToTry; // When a name is resolved many addresses can be returned, we will try all of them until a connection is successfuly established.

      QList< QSharedPointer<BrowseResult> > browseResultsWithoutTag;
      QList< QSharedPointer<SearchResult> > searchResultsWithoutTag;
      bool authenticated;
   };
}

#endif
