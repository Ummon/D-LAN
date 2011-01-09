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
  
#include <Tests.h>
using namespace DM;

#include <QtDebug>
#include <QStringList>

#include <Protos/core_settings.pb.h>
#include <Protos/core_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/Network.h>
#include <Common/ZeroCopyStreamQIODevice.h>
#include <Common/Settings.h>

#include <IDownload.h>

const int Tests::PORT = 59487;

Tests::Tests()
{
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();
   qDebug() << "===== initTestCase() =====";

   Common::PersistentData::rmValue(Common::FILE_CACHE, Common::Global::LOCAL); // Reset the stored cache.
   Common::PersistentData::rmValue(Common::FILE_QUEUE, Common::Global::LOCAL); // Reset the stored queue.

   SETTINGS.setFilename("core_settings.txt");
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());

   this->createInitialFiles();

   this->fileManagers <<
      FM::Builder::newFileManager() <<
      FM::Builder::newFileManager() <<
      FM::Builder::newFileManager();

   this->peerManagers <<
      PM::Builder::newPeerManager(this->fileManagers[0]) <<
      PM::Builder::newPeerManager(this->fileManagers[1]) <<
      PM::Builder::newPeerManager(this->fileManagers[2]);

   this->fileManagers[0]->setSharedDirsReadWrite(QStringList() << QDir::currentPath().append("/sharedDirs/peer1/incoming"));
   this->fileManagers[0]->setSharedDirsReadOnly(QStringList() << QDir::currentPath().append("/sharedDirs/peer1/shared"));
   this->fileManagers[1]->setSharedDirsReadOnly(QStringList() << QDir::currentPath().append("/sharedDirs/peer2"));
   this->fileManagers[2]->setSharedDirsReadOnly(QStringList() << QDir::currentPath().append("/sharedDirs/peer3"));

   this->uploadManagers <<
      UM::Builder::newUploadManager(this->peerManagers[0]) <<
      UM::Builder::newUploadManager(this->peerManagers[1]) <<
      UM::Builder::newUploadManager(this->peerManagers[2]);

   this->downloadManagers <<
      Builder::newDownloadManager(this->fileManagers[0], this->peerManagers[0]) <<
      Builder::newDownloadManager(this->fileManagers[1], this->peerManagers[1]) <<
      Builder::newDownloadManager(this->fileManagers[2], this->peerManagers[2]);

   this->peerUpdater = new PeerUpdater(this->fileManagers, this->peerManagers, PORT);

   this->servers <<
      new TestServer(this->peerManagers[0], PORT) <<
      new TestServer(this->peerManagers[1], PORT + 1) <<
      new TestServer(this->peerManagers[2], PORT + 2);

   this->peerUpdater->start();
}

void Tests::addADirectoryToDownload()
{
   qDebug() << "===== addADirectoryToDownload() =====";

   Protos::Common::Entries result = this->fileManagers[1]->getEntries();
   this->downloadManagers[0]->addDownload(result.entry(0), this->peerManagers[1]->getID());
   QTest::qWait(1000);
}

void Tests::addABigFileToDownload()
{
   qDebug() << "===== addABigFileToDownload() =====";

   const int FILE_SIZE = 128 * 1024 * 1024;
   {
      QFile file("sharedDirs/peer2/big.bin");
      file.open(QIODevice::WriteOnly);

      // To have four different hashes.
      for (int i = 0; i < 4 ; i++)
      {
         QByteArray randomData(32 * 1024 * 1024, i+1);
         file.write(randomData);
      }
   }
   QTest::qWait(4000);

   Protos::Common::Entries rootEntry = this->fileManagers[1]->getEntries();

   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry_Type_FILE);
   entry.set_path("/");
   entry.set_name("big.bin");
   entry.set_size(FILE_SIZE);
   entry.mutable_shared_dir()->CopyFrom(rootEntry.entry(0).shared_dir());

   this->downloadManagers[0]->addDownload(entry, this->peerManagers[1]->getID());

   for (int i = 0; i < 15; i++)
   {
      qDebug() << "Upload rate : " << QString("%1/s").arg(Common::Global::formatByteSize(this->uploadManagers[1]->getUploadRate()));
      qDebug() << "Download rate : " << QString("%1/s").arg(Common::Global::formatByteSize(this->downloadManagers[0]->getDownloadRate()));

      this->printDownloads(0);

      QTest::qWait(100);
   }
}

/*void Tests::addABigFileToDownloadFromTwoPeers()
{
   qDebug() << "===== addABigFileToDownloadFromTwoPeers() =====";

   const int FILE_SIZE = 256 * 1024 * 1024;
   QStringList files;
   files << "sharedDirs/peer2/big2.bin";
   files << "sharedDirs/peer3/anotherBigFile.bin";
   for (QStringListIterator i(files); i.hasNext();)
   {
      QFile file(i.next());
      file.open(QIODevice::WriteOnly);

      // To have height different hashes.
      for (int i = 0; i < 8 ; i++)
      {
         QByteArray randomData(32 * 1024 * 1024, i+1);
         file.write(randomData);
      }
   }
   QTest::qWait(5000);

   // TODO : get

}*/

void Tests::cleanupTestCase()
{
   QTest::qWait(1000);
   qDebug() << "===== cleanupTestCase() =====";

   for (QListIterator<TestServer*> i(this->servers); i.hasNext();)
      delete i.next();
   delete this->peerUpdater;
}

void Tests::printDownloads(int num)
{
   QList<IDownload*> downloads = this->downloadManagers[num]->getDownloads();
   for (QListIterator<IDownload*> i(downloads); i.hasNext();)
   {
      IDownload* download = i.next();
      qDebug() << QString("[%1] : status = %2, progress = %3").arg(download->getID()).arg(download->getStatus()).arg(download->getProgress());
   }
}

void Tests::createInitialFiles()
{
   this->deleteAllFiles();

   Common::Global::createFile("sharedDirs/peer1/incoming/");
   Common::Global::createFile("sharedDirs/peer1/shared/");

   Common::Global::createFile("sharedDirs/peer2/subdir/a.txt");
   Common::Global::createFile("sharedDirs/peer2/subdir/b.txt");
   Common::Global::createFile("sharedDirs/peer2/subdir/c.txt");
   Common::Global::createFile("sharedDirs/peer2/d.txt");
   Common::Global::createFile("sharedDirs/peer2/e.txt");
   Common::Global::createFile("sharedDirs/peer2/f.txt");

   Common::Global::createFile("sharedDirs/peer3/a.txt");
   Common::Global::createFile("sharedDirs/peer3/b.txt");
   Common::Global::createFile("sharedDirs/peer3/c.txt");
}

void Tests::deleteAllFiles()
{
   Common::Global::recursiveDeleteDirectory("sharedDirs");
}

