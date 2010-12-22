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
  
#ifndef TESTS_DOWNLOADMANAGER_TESTS_H
#define TESTS_DOWNLOADMANAGER_TESTS_H

#include <QTest>
#include <QSharedPointer>
#include <QtNetwork>

#include <Core/FileManager/Builder.h>
#include <Core/FileManager/IFileManager.h>

#include <Core/PeerManager/Builder.h>
#include <Core/PeerManager/IPeerManager.h>

#include <Core/UploadManager/Builder.h>
#include <Core/UploadManager/IUploadManager.h>

#include <Builder.h>
#include <IDownloadManager.h>
using namespace DM;

#include <Core/PeerManager/TestsPeerManager/TestServer.h>
#include <Core/PeerManager/TestsPeerManager/PeerUpdater.h>

class Tests : public QObject
{
   Q_OBJECT
   static const int PORT;

public:
   Tests();

private slots:
   void initTestCase();
   void addADirectoryToDownload();
   void addABigFileToDownload();
   //void addABigFileToDownloadFromTwoPeers();
   void cleanupTestCase();

private:
   void printDownloads(int num);
   void createInitialFiles();
   void deleteAllFiles();

   QList< QSharedPointer<FM::IFileManager> > fileManagers;
   QList< QSharedPointer<PM::IPeerManager> > peerManagers;
   QList< QSharedPointer<UM::IUploadManager> > uploadManagers;
   QList< QSharedPointer<DM::IDownloadManager> > downloadManagers;

   QList<TestServer*> servers;

   PeerUpdater* peerUpdater;
};

#endif
