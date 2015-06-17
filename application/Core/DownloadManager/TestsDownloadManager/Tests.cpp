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
  
#include <Tests.h>
using namespace DM;

#include <QtDebug>
#include <QStringList>

#include <Protos/core_protocol.pb.h>
#include <Protos/core_settings.pb.h>
#include <Protos/common.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/Global.h>

#include <Builder.h>

/**
  * @class Tests
  *
  */

Tests::Tests()
{
}

void Tests::initTestCase()
{
   qDebug() << Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL, false);

   LM::Builder::initMsgHandler();
   qDebug() << "===== initTestCase() =====";

   this->fileManager = QSharedPointer<MockFileManager>(new MockFileManager());
   this->peerManager = QSharedPointer<MockPeerManager>(new MockPeerManager());
   this->downloadManager = Builder::newDownloadManager(this->fileManager, this->peerManager);
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";
}

