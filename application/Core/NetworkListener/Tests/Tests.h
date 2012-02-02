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
  
#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QSharedPointer>

#include <INetworkListener.h>

#include <Common/LogManager/ILogger.h>
#include <FileManager/IFileManager.h>
#include <NetworkListener/INetworkListener.h>
#include <NetworkListener/IChat.h>
#include <PeerManager/IPeerManager.h>

using namespace NL;

namespace DM { class IDownloadManager; }
namespace UM { class IUploadManager; }

class Tests : public QObject
{
Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();
   void testSending();
   void testReception();
   void messageRecevied(const Protos::Core::ChatMessage& message);


private :
   QSharedPointer<LM::ILogger> logger;
   QSharedPointer<FM::IFileManager> fileManager;
   QSharedPointer<NL::INetworkListener> networkListener;
   QSharedPointer<PM::IPeerManager> peerManager;
   bool isMessageRecevied;
};

#endif
