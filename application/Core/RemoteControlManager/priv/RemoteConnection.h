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
  
#ifndef REMOTECONTROLMANAGER_REMOTECONNECTION_H
#define REMOTECONTROLMANAGER_REMOTECONNECTION_H

#include <QObject>
#include <QSharedPointer>
#include <QTcpSocket>
#include <QTimer>
#include <QList>
#include <QLocale>

#include <Libs/MersenneTwister.h>

#include <google/protobuf/message.h>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/Uncopyable.h>
#include <Common/Network/MessageHeader.h>
#include <Common/Network/MessageSocket.h>
#include <Common/LogManager/Builder.h>
#include <Common/LogManager/IEntry.h>
#include <Common/LogManager/ILogger.h>
#include <Common/LogManager/ILoggerHook.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/PeerManager/IGetEntriesResult.h>
#include <Core/UploadManager/IUploadManager.h>
#include <Core/DownloadManager/IDownloadManager.h>
#include <Core/NetworkListener/INetworkListener.h>
#include <Core/NetworkListener/ISearch.h>

namespace RCM
{
   class RemoteConnection : public Common::MessageSocket
   {
      Q_OBJECT

      static const int MAX_DELAY_WAITING_AUTH_RES = 5000; // [s]. We close the socket if we don't receive a response after this delay when sending the message 'Protos.GUI.AskForAuthentication'.

   protected:
      class Logger : public ILogger
      {
      public:
         void logDebug(const QString& message);
         void logError(const QString& message);
      };

   public:
      RemoteConnection(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<UM::IUploadManager> uploadManager,
         QSharedPointer<DM::IDownloadManager> downloadManager,
         QSharedPointer<NL::INetworkListener> networkListener,
         QTcpSocket* socket
      );
      ~RemoteConnection();

      void send(Common::MessageHeader::MessageType type, const google::protobuf::Message& message);

      void sendMessageToItself(const QString& message);

   signals:
      void deleted(RemoteConnection*);
      void chatMessageSent(const QString&);
      void languageDefined(QLocale);

   private slots:
      void refresh();
      void closeSocket();

      void newChatMessage(const Protos::GUI::EventChatMessages_Message& message);
      void searchFound(const Protos::Common::FindResult& result);

      void getEntriesResult(const Protos::Core::GetEntriesResult&);
      void getEntriesTimeout();

      void newLogEntry(QSharedPointer<const LM::IEntry> entry);

      void sendNoPasswordDefinedResult();
      void sendBadPasswordResult();

   private:
      void askForAuthentication();
      void removeGetEntriesResult(const PM::IGetEntriesResult* getEntriesResult);
      void sendLastChatMessages();

      void onNewMessage(Common::MessageHeader::MessageType type, const google::protobuf::Message& message);
      void onDisconnected();

      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<UM::IUploadManager> uploadManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;
      QSharedPointer<NL::INetworkListener> networkListener;

      QSharedPointer<LM::ILoggerHook> loggerHook;

      QTimer timerRefresh;
      QTimer timerCloseSocket;

      QList< QSharedPointer<NL::ISearch> > currentSearches;
      QList< QSharedPointer<PM::IGetEntriesResult> > getEntriesResults;

      MTRand mtrand;

      bool authenticated;
      quint64 saltChallenge;

#ifdef DEBUG
      QSharedPointer<LM::ILogger> loggerRefreshState; // A logger especially for the state message.
#endif
   };
}
#endif
