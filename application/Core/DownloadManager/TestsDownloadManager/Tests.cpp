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
#include <QTemporaryDir>
#include <Common/Constants.h>
#include <Core/FileManager/Exceptions.h>
#include <Core/FileManager/priv/Exceptions.h>
#include <Core/FileManager/priv/Cache/Cache.h>
#include <Core/FileManager/priv/Cache/File.h>
#include <Core/PeerManager/priv/Peer.h>
#include <priv/FileDownload.h>

namespace
{
   class EmptyHashCache : public HC::IHashCache
   {
   public:
      QList<Common::Hash> getHashes(const QString&, QDateTime) override { return {}; }
      void setHashes(const QString&, const QList<Common::Hash>&, qint64, QDateTime) override {}
      void rmHashes(const QString&) override {}
   };

   class ResumeFileManager : public MockFileManager
   {
   public:
      QList<QSharedPointer<FM::IChunk>> chunks;
      QList<QSharedPointer<FM::IChunk>> getAllChunks(const Protos::Common::Entry&,
         const QList<Common::Hash>&) const override { return this->chunks; }
   };

   class ResumePeer : public PM::Peer
   {
   public:
      ResumePeer(QSharedPointer<FM::IFileManager> files) : Peer(nullptr, files, Common::Hash::rand(), "source") {}
      bool isAvailable() const override { return true; }
   };
}

void Tests::resumeMissingFile_data()
{
   QTest::addColumn<bool>("removeFile");
   QTest::newRow("deleted") << true;
   QTest::newRow("still-present") << false;
}

void Tests::resumeMissingFile()
{
   QFETCH(bool, removeFile);
   FM::Chunk::CHUNK_SIZE = Common::Constants::CHUNK_SIZE;
   QTemporaryDir temp;
   QVERIFY(temp.isValid());
   FM::Cache cache(QSharedPointer<HC::IHashCache>(new EmptyHashCache));
   const auto shared = cache.addASharedPath(temp.path() + '/');
   auto root = dynamic_cast<FM::SharedDirectory*>(cache.getSharedEntry(shared.first.ID));
   QVERIFY(root);
   const qint64 size = qint64(FM::Chunk::CHUNK_SIZE) + 10;
   const QList<Common::Hash> hashes { Common::Hash::rand(), Common::Hash::rand() };
   auto file = new FM::File(root, "resumed.bin", size, false, QDateTime::currentDateTime(),
      root->getRootDir(), hashes, true);
   const auto chunks = file->getChunks();
   chunks[0]->setKnownBytes(FM::Chunk::CHUNK_SIZE); // A previously completed chunk must reset too.
   chunks[1]->setKnownBytes(5);
   const QString path = file->getAbsolutePath();
   QSharedPointer<ResumeFileManager> files(new ResumeFileManager);
   for (const auto& chunk : chunks)
      files->chunks << chunk;
   ResumePeer peer(files);
   DM::LinkedPeers links;
   DM::OccupiedPeers asking, downloading;
   Common::ThreadPool pool(1);
   Common::TransferRateCalculator rate;
   Protos::Common::Entry entry;
   file->populateEntry(&entry, true);
   entry.set_name("resumed.bin");
   DM::FileDownload download(files, links, asking, downloading, pool, &peer, entry, entry,
      rate, Protos::Queue::Queue::Entry::PAUSED);
   download.start();
   QCOMPARE(download.getDownloadedBytes(), quint64(size - 5));
   if (removeFile)
      QVERIFY(QFile::remove(path));
   QVERIFY(download.pause(false));
   QVERIFY(QFileInfo::exists(path));
   QCOMPARE(QFileInfo(path).size(), size);
   QCOMPARE(download.getDownloadedBytes(), removeFile ? quint64(0) : quint64(size - 5));
   auto next = download.getAChunkToDownload();
   QVERIFY(next);
   QCOMPARE(next->getChunk()->getKnownBytes(), removeFile ? 0 : 5);
   if (removeFile)
      for (const auto& chunk : chunks)
      {
         QCOMPARE(chunk->getKnownBytes(), 0);
         QVERIFY(!chunk->isComplete());
      }
   Protos::Queue::Queue::Entry saved;
   download.populateQueueEntry(&saved);
   QCOMPARE(saved.local_entry().shared_entry().id().hash(), entry.shared_entry().id().hash());
   if (removeFile)
      for (auto knownBytes : saved.known_bytes())
         QCOMPARE(knownBytes, 0);
}

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
