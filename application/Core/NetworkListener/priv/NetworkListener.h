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
  
#ifndef NETWORKLISTENER_NETWORKLISTENER_H
#define NETWORKLISTENER_NETWORKLISTENER_H

#include <QObject>
#include <QSharedPointer>

#include <Common/Uncopyable.h>
#include <Core/FileManager/IFileManager.h>
#include <Core/PeerManager/IPeerManager.h>
#include <Core/DownloadManager/IDownloadManager.h>

#include <INetworkListener.h>
#include <ISearch.h>
#include <priv/UDPListener.h>
#include <priv/TCPListener.h>
#include <priv/Chat.h>

namespace NL
{
   class NetworkListener : public QObject, public INetworkListener, Common::Uncopyable
   {
      Q_OBJECT
   public:
      NetworkListener(
         QSharedPointer<FM::IFileManager> fileManager,
         QSharedPointer<PM::IPeerManager> peerManager,
         QSharedPointer<DM::IDownloadManager> downloadManager
      );

      ~NetworkListener();

      IChat& getChat();
      QSharedPointer<ISearch> newSearch();

   private:
      QSharedPointer<FM::IFileManager> fileManager;
      QSharedPointer<PM::IPeerManager> peerManager;
      QSharedPointer<DM::IDownloadManager> downloadManager;

      TCPListener tCPListener;
      UDPListener uDPListener;
      Chat chat;
   };
}
#endif
