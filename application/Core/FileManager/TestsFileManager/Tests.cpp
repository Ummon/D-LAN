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
using namespace FM;

#include <QtDebug>
#include <QRegularExpression>
#include <QFile>
#include <QElapsedTimer>
#include <QTextStream>
#include <QDataStream>
#include <QStringList>
#include <QDirIterator>

#include <Common/LogManager/Builder.h>
#include <Common/PersistentData.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/ProtoHelper.h>
#include <Common/SharedEntry.h>

#include <IChunk.h>
#include <IDataWriter.h>
#include <IGetHashesResult.h>
#include <Exceptions.h>
#include <priv/Constants.h>

#include <HashesReceiver.h>
#include <Utils.h>

/**
  * The nu script 'application/Tools/compute_chunk_hash.nu' can be use to compute a given chunk,
  * for example to compute the second chunk (n=1) of the file 'sharedDirs/big.bin':
  * $> source compute_chunk_hash.nu
  * $> chunk-hash `sharedDirs/big.bin` 2
  */

Tests::Tests() :
   testBigFiles(false)
{   
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();

   qDebug() << "===== initTestCase() =====";

   try
   {
      QString tempFolder = Common::Global::setCurrentDirToTemp("FileManagerTests");
      qDebug() << "Application folder path (where the persistent data is put) : " <<
         Common::Global::getDataFolder(Common::Global::DataFolderType::LOCAL, false);
      qDebug() << "The file created during this test are put in : " << tempFolder;
   }
   catch (Common::Global::UnableToSetTempDirException& e)
   {
      QFAIL(qUtf8Printable(e.errorMessage));
   }

   this->createInitialFiles();

   // Reset the stored cache.
   // Common::PersistentData::rmValue(Common::Constants::FILE_CACHE, Common::Global::DataFolderType::LOCAL);
}

void Tests::createFileManager()
{
   qDebug() << "===== createFileManager() =====";

   this->hashCache = QSharedPointer<HC::IHashCache>(new MockHashCache());
   this->fileManager = Builder::newFileManager(this->hashCache);
}

void Tests::addASharedDirectoryIncoming()
{
   qDebug() << "===== addASharedDirectoryIncoming() =====";

   this->sharedPaths << IFileManager::SharedPath{ QString("incoming"), QDir::currentPath().append("/incoming/") };
   this->fileManager->setSharedPaths(this->sharedPaths);

   QList<Common::SharedEntry> paths = this->fileManager->getSharedEntries();
   QVERIFY(paths.size() == 1);
   QCOMPARE(paths.at(0).path.toString(), this->sharedPaths.at(0).path);
}

void Tests::addASharedDirectory()
{
   qDebug() << "===== addASharedDirectory() =====";

   this->sharedPaths << IFileManager::SharedPath{ QString("share1"), QDir::currentPath().append("/sharedDirs/share1/") };
   this->fileManager->addASharedPath(this->sharedPaths.last().path);
   QList<Common::SharedEntry> paths = this->fileManager->getSharedEntries();
   QVERIFY(paths.size() == 2);
   QCOMPARE(paths.at(1).path.toString(), this->sharedPaths.at(1).path);
}

void Tests::addTwoSameSharedDirectories()
{
   qDebug() << "===== addTwoSameSharedDirectories() =====";

   // "share1" already exists in sharedPaths.
   this->sharedPaths << IFileManager::SharedPath{ QString("share1 with another user name"), QDir::currentPath().append("/sharedDirs/share1/") };
   this->fileManager->setSharedPaths(this->sharedPaths);
   QList<Common::SharedEntry> paths = this->fileManager->getSharedEntries();
   QVERIFY(paths.size() == 2);

   this->sharedPaths.removeLast();
   QCOMPARE(paths.at(1).path.toString(), this->sharedPaths.at(1).path);
}

void Tests::addASharedFile()
{
   qDebug() << "===== addASharedFile() =====";

   this->sharedPaths << IFileManager::SharedPath{ QString("shared file.txt"), QDir::currentPath().append("/shared file.txt") };
   this->fileManager->setSharedPaths(this->sharedPaths);
   QList<Common::SharedEntry> paths = this->fileManager->getSharedEntries();
   QVERIFY(paths.size() == 3);
   QCOMPARE(paths.at(2).path.toString(), this->sharedPaths.at(2).path);
}

void Tests::addSomeAlreadySharedEntries()
{
   qDebug() << "===== addSomeAlreadySharedEntries() =====";

   this->fileManager->setSharedPaths(this->sharedPaths);
   QList<Common::SharedEntry> paths = this->fileManager->getSharedEntries();
   QVERIFY(paths.size() == 3);
   for (int i = 0; i < paths.size(); i++)
      QCOMPARE(paths.at(i).path.toString(), this->sharedPaths.at(i).path);
}

void Tests::swapTwoDirectories()
{
   qDebug() << "===== swapTwoDirectories() =====";

   this->sharedPaths.move(1, 0);
   this->fileManager->setSharedPaths(this->sharedPaths);
   QList<Common::SharedEntry> paths = this->fileManager->getSharedEntries();
   QCOMPARE(paths.at(0).path.toString(), this->sharedPaths.at(0).path);
   QCOMPARE(paths.at(1).path.toString(), this->sharedPaths.at(1).path);

   this->sharedPaths.move(1, 0);
   this->fileManager->setSharedPaths(this->sharedPaths);
   QList<Common::SharedEntry> paths2 = this->fileManager->getSharedEntries();
   QCOMPARE(paths2.at(0).path.toString(), this->sharedPaths.at(0).path);
   QCOMPARE(paths2.at(1).path.toString(), this->sharedPaths.at(1).path);
}

void Tests::addInexistingSharedDirectory()
{
   qDebug() << "===== addInexistingSharedDirectory() =====";

   this->sharedPaths << IFileManager::SharedPath{ QString(), QDir::currentPath().append("/this_is_spartaaaaaa/") }; // This directory doesn't exit.
   try
   {
      this->fileManager->setSharedPaths(this->sharedPaths);
      QFAIL("An exception must be thrown");
   }
   catch (EntriesNotFoundException& e)
   {
      QVERIFY(e.paths.size() == 1);
      QCOMPARE(e.paths.at(0), this->sharedPaths.last().path);
      qDebug() << "This directory hasn't been found: " << e.paths.at(0) << " (Exception thrown)";
   }
   this->sharedPaths.removeLast();
}

void Tests::addInexistingSharedFile()
{
   qDebug() << "===== addInexistingSharedFile() =====";

   this->sharedPaths << IFileManager::SharedPath{ QString(), QDir::currentPath().append("/inexisting file.txt") }; // This directory doesn't exit.
   try
   {
      this->fileManager->setSharedPaths(this->sharedPaths);
      QFAIL("An exception must be thrown");
   }
   catch (EntriesNotFoundException& e)
   {
      QVERIFY(e.paths.size() == 1);
      QCOMPARE(e.paths.at(0), this->sharedPaths.last().path);
      qDebug() << "This file hasn't been found: " << e.paths.at(0) << " (Exception thrown)";
   }
   this->sharedPaths.removeLast();
}

/**
  * Adding sub shared directory should be ineffective.
  */
void Tests::addSubSharedDirectories()
{
   qDebug() << "===== addSubSharedDirectories() =====";

   this->sharedPaths << IFileManager::SharedPath{ QString(), QDir::currentPath().append("/sharedDirs/share1/subdir/") };
   this->sharedPaths << IFileManager::SharedPath{ QString(), QDir::currentPath().append("/sharedDirs/share1/another subdir/") };

   this->fileManager->setSharedPaths(this->sharedPaths);

   auto sharedEntries = this->fileManager->getSharedEntries();
   QCOMPARE(sharedEntries.size(), 3);
   QCOMPARE(sharedEntries[0].getName(), "incoming");
   QCOMPARE(sharedEntries[1].getName(), "share1");
   QCOMPARE(sharedEntries[2].getName(), "shared file.txt");

   this->sharedPaths.removeLast();
   this->sharedPaths.removeLast();
}

/**
  * The subs directories of each subdirectory must be merged into the super directory.
  */
void Tests::addSuperSharedDirectories()
{
   qDebug() << "===== addSuperSharedDirectories() =====";

   this->sharedPaths << IFileManager::SharedPath{ QString("sharedDirs"), QDir::currentPath().append("/sharedDirs/") };

   this->fileManager->setSharedPaths(this->sharedPaths);

   auto sharedEntries = this->fileManager->getSharedEntries();
   QCOMPARE(sharedEntries.size(), 3);
   QCOMPARE(sharedEntries[0].getName(), "incoming");
   QCOMPARE(sharedEntries[1].getName(), "shared file.txt");
   QCOMPARE(sharedEntries[2].getName(), "sharedDirs");
}

void Tests::createAFile()
{
   qDebug() << "===== createAFile() =====";

   Common::Global::createFile("sharedDirs/x.txt");

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (entry.name() == "x.txt")
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::moveAFile()
{
   qDebug() << "===== moveAFile() =====";

   QDir::current().rename("sharedDirs/x.txt", "sharedDirs/share1/x.txt");

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (entry.name() == "x.txt")
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::renameAFile()
{
   qDebug() << "===== renameAFile() =====";

   QDir::current().rename("sharedDirs/share1/x.txt", "sharedDirs/share1/y.txt");

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (entry.name() == "y.txt")
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::modifyAFile()
{
   qDebug() << "===== modifyAFile() =====";

   {
      QFile file("sharedDirs/share1/y.txt");
      file.open(QIODevice::Append);
      QTextStream stream(&file);
      stream << "12345";
   }

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (entry.name() == "y.txt" && entry.chunks().size() == 1)
               {
                  const auto hash = Common::Hash(entry.chunks().Get(0).hash());
                  return hash.toStr() == "58bc9937ff71885bf52f92746c1a85447a81b9e959f35c1d00bd7dca";
               }
            }
            return false;
         }
      )
   );
}

void Tests::removeAFile()
{
   qDebug() << "===== removeAFile() =====";

   QFile("sharedDirs/share1/y.txt").remove();

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (entry.name() == "y.txt")
                  return false;
            }
            return true;
         }
      )
   );
}

void Tests::createASubFile()
{
   qDebug() << "===== createASubFile() =====";

   Common::Global::createFile("sharedDirs/share1/v.txt");

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 200,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "v.txt" &&
                  entry.chunks().size() == 1 &&
                  Common::Hash(entry.chunks(0).hash()).toStr() == "7b6f7f3309179b97b88de3c178274b7e38343267bcdfe653c819593e"
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::createABigFile()
{
   if (!this->testBigFiles)
      return;

   qDebug() << "===== createABigFile() =====";
   auto size = 128 * 1024 * 1024; // 128Mo.
   auto nbChunks = size / Common::Constants::CHUNK_SIZE;

   {
      const QString filePath("sharedDirs/big.bin");
      QFile file(filePath);
      if (!Utils::tryOpen(file, QIODevice::ReadWrite))
      {
         qDebug() << "Can't open the file " << filePath;
         return;
      }
      file.resize(size);
   }

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(10, 500,
         [this, &sharedEntry, &nbChunks, &size]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "big.bin" &&
                  entry.size() == size &&
                  entry.chunks_size() == nbChunks &&
                  Common::Hash(entry.chunks(0).hash()).toStr() == "ea7b156fc9a810c181984f9e2da433feeeb2bf88ffa4d1f0dc1a9215" &&
                  Common::Hash(entry.chunks(1).hash()).toStr() == "ea7b156fc9a810c181984f9e2da433feeeb2bf88ffa4d1f0dc1a9215"
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::modifyABigFile()
{
   if (!this->testBigFiles)
      return;

    // Write at the middle of the first data chunk.
   qDebug() << "===== modifyABigFile() =====";

   auto size = 128 * 1024 * 1024; // 128Mo.
   auto nbChunks = size / Common::Constants::CHUNK_SIZE;

   {
      const QString filePath("sharedDirs/big.bin");
      QFile file(filePath);
      if (!Utils::tryOpen(file, QIODevice::ReadWrite))
      {
         qDebug() << "Can't open the file " << filePath;
         return;
      }
      QDataStream stream(&file);
      stream.skipRawData(32 * 1024 * 1024 - 3);
      QByteArray data("XXXXXX");
      stream.writeRawData(data.constData(), data.size());
   }

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(10, 500,
         [this, &sharedEntry, &nbChunks, &size]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "big.bin" &&
                  entry.size() == size &&
                  entry.chunks_size() == nbChunks &&
                  Common::Hash(entry.chunks(0).hash()).toStr() == "93bcb2f6063a716b2a37ddbee031d07851811b72f21d5ece028b5544" &&
                  Common::Hash(entry.chunks(1).hash()).toStr() == "ea7b156fc9a810c181984f9e2da433feeeb2bf88ffa4d1f0dc1a9215"
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::modifyABigFile2()
{
   if (!this->testBigFiles)
      return;

   qDebug() << "===== modifyABigFile2() =====";

   // Add data at the end of the file.
   auto size = 128 * 1024 * 1024 + 6; // 128Mo + 6 bytes.
   int nbChunks = ceil(double(size) / double(Common::Constants::CHUNK_SIZE));

   {
      const QString filePath("sharedDirs/big.bin");
      QFile file(filePath);
      if (!Utils::tryOpen(file, QIODevice::ReadWrite))
      {
         qDebug() << "Can't open the file " << filePath;
         return;
      }
      QDataStream stream(&file);
      stream.skipRawData(file.size());
      QByteArray data("AAAAAA");
      stream.writeRawData(data.constData(), data.size());
   }

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(10, 500,
         [this, &sharedEntry, &nbChunks, &size]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "big.bin" &&
                  entry.size() == size &&
                  entry.chunks_size() == nbChunks &&
                  Common::Hash(entry.chunks(0).hash()).toStr() == "93bcb2f6063a716b2a37ddbee031d07851811b72f21d5ece028b5544" &&
                  Common::Hash(entry.chunks(1).hash()).toStr() == "ea7b156fc9a810c181984f9e2da433feeeb2bf88ffa4d1f0dc1a9215" &&
                  Common::Hash(entry.chunks(2).hash()).toStr() == "02fc503da5cd886aeed611e42b654f9b1e178865e03847377a796250"
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::modifyABigFile3()
{
   if (!this->testBigFiles)
      return;

   qDebug() << "===== modifyABigFile3() =====";

   auto size = 64 * 1024 * 1024 + 10; // 64Mo + 10 bytes (2 chunks).
   int nbChunks = ceil(double(size) / double(Common::Constants::CHUNK_SIZE));

   {
      const QString filePath("sharedDirs/big.bin");
      QFile file(filePath);
      if (!Utils::tryOpen(file, QIODevice::ReadWrite))
      {
         qDebug() << "Can't open the file " << filePath;
         return;
      }
      file.resize(size); // Truncates the file.
   }

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(10, 500,
         [this, &sharedEntry, &nbChunks, &size]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "big.bin" &&
                  entry.size() == size &&
                  entry.chunks_size() == nbChunks &&
                  Common::Hash(entry.chunks(0).hash()).toStr() == "93bcb2f6063a716b2a37ddbee031d07851811b72f21d5ece028b5544" &&
                  Common::Hash(entry.chunks(1).hash()).toStr() == "40772e14b7665a8e7f09de41da09c4191acac132a598e4e363d076e1"
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::removeABigFile()
{
   if (!this->testBigFiles)
      return;

   qDebug() << "===== removeABigFile() =====";

   while(!QFile("sharedDirs/big.bin").remove())
      QTest::qWait(100);

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(10, 500,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
               if (entry.name() == "big.bin")
                  return false;
            return true;
         }
      )
   );
}

void Tests::createADirectory()
{
   qDebug() << "===== createADirectory() =====";

   Common::Global::createFile("sharedDirs/a/");

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "a" &&
                  entry.type() == Protos::Common::Entry::Type::Entry_Type_DIR
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::renameADirectory()
{
   qDebug() << "===== renameADirectory() =====";

   while (!QDir("sharedDirs").rename("a", "b"))
      QTest::qWait(100);

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "b" &&
                  entry.type() == Protos::Common::Entry::Type::Entry_Type_DIR
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::moveAnEmptyDirectory()
{
   qDebug() << "===== moveAnEmptyDirectory() =====";

   QDir("sharedDirs").rename("b", "share1/b");

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "b" &&
                  entry.type() == Protos::Common::Entry::Type::Entry_Type_DIR
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::moveADirectoryContainingFiles()
{
   qDebug() << "===== moveADirectoryContainingFiles() =====";

   QDir("sharedDirs").rename("share2", "share1/share2");

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "share2" &&
                  entry.type() == Protos::Common::Entry::Type::Entry_Type_DIR
               )
                  return true;
            }
            return false;
         }
      )
   );
}

void Tests::removeADirectory()
{
   qDebug() << "===== removeADirectory() =====";

   Common::Global::recursiveDeleteDirectory("sharedDirs/share1/share2");

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/"));
   QVERIFY(sharedEntry.IsInitialized());
   QVERIFY(
      Utils::retry(5, 100,
         [this, &sharedEntry]()
         {
            auto entries = this->fileManager->getEntries(sharedEntry);
            for (const auto& entry : entries.entries())
            {
               if (
                  entry.name() == "share2" &&
                  entry.type() == Protos::Common::Entry::Type::Entry_Type_DIR
               )
                  return false;
            }
            return true;
         }
      )
   );
}

void Tests::createNewFileAndWriteData()
{
   qDebug() << "===== createNewFileAndWriteData() =====";

   Protos::Common::Entry remoteEntry;
   remoteEntry.set_path("/remoteShare1/"); // Path is ignored.
   remoteEntry.set_name("remoteFile.txt");
   remoteEntry.set_size(12);

   try
   {
      QList<QSharedPointer<IChunk>> chunks = this->fileManager->newFile(remoteEntry);
      QCOMPARE(chunks.size(), 1);
      QVERIFY(chunks[0]->getHash().isNull());

      chunks[0]->setHash(Common::Hash::fromStr("a74a542ea1f9957f55bae199f89ab46b90c8b41e940489075ec92449").value());

      auto writer = chunks[0]->getDataWriter();
      QByteArray data("abcdefghijkl", 12);
      QVERIFY_THROWS_EXCEPTION(IOErrorException, writer->write(data.constData(), -1));
      QVERIFY_THROWS_EXCEPTION(IOErrorException, writer->write(nullptr, 1));
      writer->write(data.constData(), data.size());
   }
   catch (NoWriteableDirectoryException&)
   {
      QFAIL("NoWriteableDirectoryException");
   }
   catch (InsufficientStorageSpaceException&)
   {
      QFAIL("InsufficientStorageSpaceException");
   }
   catch (UnableToCreateNewFileException&)
   {
      QFAIL("UnableToCreateNewFileException");
   }
}

void Tests::getAnExistingChunk()
{
   qDebug() << "===== getAExistingChunk() =====";

   // From 'sharedDirs/share1/subdir/p.txt'.
   QSharedPointer<IChunk> chunk =
      this->fileManager->getChunk(Common::Hash::fromStr("629c2ce8f7532e0c5e721157dd938c61f1f6e320043363ac5f243a32").value());

   if (chunk.isNull())
      QFAIL("Chunk not found");
   else
      qDebug() << "Chunk found: " << chunk->getHash().toStr();
}

void Tests::getANonExistingChunk()
{
   qDebug() << "===== getANonExistingChunk() =====";

   QSharedPointer<IChunk> chunk = this->fileManager->getChunk(Common::Hash::rand(1));
   if (chunk.isNull())
      qDebug() << "Chunk not found: OK" << Common::Hash::rand().toStr();
   else
      QFAIL("No chunk must be found");
}

void Tests::getHashesFromAFileEntry1()
{
   qDebug() << "===== getHashesFromAFileEntry1() =====";

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/share1/r.txt"));
   QVERIFY(sharedEntry.IsInitialized());

   const std::string sharedDirId = sharedEntry.shared_entry().id().hash();

   Protos::Common::Entry entry;
   entry.set_path("share1/");
   entry.set_name("r.txt");
   entry.set_size(sharedEntry.size());
   entry.mutable_shared_entry()->mutable_id()->set_hash(sharedDirId);
   entry.add_chunks();

   QSharedPointer<IGetHashesResult> result = this->fileManager->getHashes(entry);

   HashesReceiver hashesReceiver(1);
   connect(result.data(), &IGetHashesResult::nextHash, &hashesReceiver, &HashesReceiver::nextHash, Qt::QueuedConnection);
   Protos::Core::GetHashesResult res = result->start();

   QCOMPARE(res.status(), Protos::Core::GetHashesResult::OK);
   QVERIFY(
      hashesReceiver.waitToReceive(
         QList<Common::Hash>()
            << Common::Hash::fromStr("f4b8657efdd1a9c11379e23004493c4f7bb5eb190f10c696729f169f").value()
         ,
         1000
      )
   );
}

void Tests::getHashesFromAFileEntry2()
{
   qDebug() << "===== getHashesFromAFileEntry2() =====";

   {
      QFile file1("sharedDirs/big2.bin");
      QVERIFY(file1.open(QIODevice::WriteOnly));

      QFile file2("sharedDirs/big3.bin");
      QVERIFY(file2.open(QIODevice::ReadWrite));

      QVERIFY(file1.resize(128 * 1024 * 1024)); // 128 MiB.
      QVERIFY(file2.resize(128 * 1024 * 1024)); // 128 MiB.

      QDataStream stream(&file2);
      QVERIFY(stream.skipRawData(64 * 1024 * 1024 + 100) > 0); // Write some data in the second chunk.
      QByteArray data("AAAA");
      QCOMPARE(stream.writeRawData(data.constData(), data.size()), data.size());
   }

   QTest::qWait(100); // Begin the computing of the big2.bin hashes.

   auto sharedEntry = Utils::tryFindEntry(this->fileManager, Common::Path("sharedDirs/big3.bin"));
   const std::string sharedDirId = sharedEntry.shared_entry().id().hash();

   Protos::Common::Entry entry;
   entry.set_path("/");
   entry.set_name("big3.bin");
   entry.set_size(sharedEntry.size());
   entry.mutable_shared_entry()->mutable_id()->set_hash(sharedDirId);
   for (int i = 0; i < 2; i++) // 128 MiB -> 2 chunks.
      entry.add_chunks();
   QSharedPointer<IGetHashesResult> result = this->fileManager->getHashes(entry);

   HashesReceiver hashesReceiver(2);
   connect(result.data(), &IGetHashesResult::nextHash, &hashesReceiver, &HashesReceiver::nextHash);
   Protos::Core::GetHashesResult res = result->start(); // Should stop the computing of 'big2.bin' and switch to 'big3.bin'.
   QCOMPARE(res.status(), Protos::Core::GetHashesResult::OK);
   QVERIFY(
      hashesReceiver.waitToReceive(
         QList<Common::Hash>()
            << Common::Hash::fromStr("ea7b156fc9a810c181984f9e2da433feeeb2bf88ffa4d1f0dc1a9215").value()
            << Common::Hash::fromStr("8bac0e4c2a03b716567e2f5dc33ca8c658dcdfa814f112ada0da4a4f").value()
         ,
         1000
      )
   );
}

void Tests::browseSomeDirectories()
{
   qDebug() << "===== browseSomeDirectories() =====";

   // Here we only search some elements in the debug serialized protos.

   // Get the shared directories.
   Protos::Common::Entries entries1 = this->fileManager->getEntries();
   QString entries1Str = Common::ProtoHelper::getDebugStr(entries1);
   qDebug().noquote() << "entries1Str: " << entries1Str;

   QCOMPARE(entries1.entries_size(), 3);
   QVERIFY(entries1Str.contains("\"shared_name\": \"incoming\""));
   QVERIFY(entries1Str.contains("\"shared_name\": \"shared file.txt\""));
   QVERIFY(entries1Str.contains("\"shared_name\": \"sharedDirs\""));

   // Ask for the files and directories of the thrid shared directory
   Protos::Common::Entries entries2 = this->fileManager->getEntries(entries1.entries(2));
   {
      QCOMPARE(entries2.entries_size(), 4);
      QString entries2Str = Common::ProtoHelper::getDebugStr(entries2);
      qDebug().noquote() << "entries2Str: " << entries2Str;
      QVERIFY(entries2Str.contains("\"name\": \"share3\""));
      const QString hashAsBase64 = QByteArray::fromHex("8bac0e4c2a03b716567e2f5dc33ca8c658dcdfa814f112ada0da4a4f").toBase64();
      QVERIFY(entries2Str.contains(QString("\"hash\": \"%1\"").arg(hashAsBase64)));
   }

   // Ask for the files and directores of the second directory of the thrid shared directory
   Protos::Common::Entries entries3 = this->fileManager->getEntries(entries2.entries(1));
   {
      QCOMPARE(entries3.entries_size(), 13);
      QString entries3Str = Common::ProtoHelper::getDebugStr(entries3);
      qDebug().noquote() << "entries3Str: " << entries3Str;
      QVERIFY(entries3Str.contains("\"name\": \"bbbb cccc.nfo\""));
      const QString hashAsBase64 = QByteArray::fromHex("8cd9bf6803aa7f1acfb6095ad5ed6db68f5de6a7eacfa10a65da81e6").toBase64();
      QVERIFY(entries3Str.contains(QString("\"hash\": \"%1\"").arg(hashAsBase64)));
   }
}

void Tests::findExistingFilesWithOneWord()
{
   qDebug() << "===== findExistingFilesWithOneWord() =====";

   QString terms("aaaa");

   FindResult expectedResult;
   expectedResult[0] << "aaaa cccc.txt" << "aaaa bbbb.txt" << "aaaa bbbb cccc.txt" << "aaaa dddddd.txt";
   expectedResult[1] << "aaaaaa dddddd.txt" << "aaaaaa bbbb.txt" << "aaaaaa bbbbbb.txt";

   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, 65536);
   QVERIFY(!results.isEmpty());
   this->printSearch(terms, results.first());
   this->compareExpectedResult(results.first(), expectedResult);
}

void Tests::findNonExistingFilesWithOneWord()
{
   qDebug() << "===== findNonExistingFilesWithOneWord() =====";

   QString terms("mmmm");
   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, 65536);
   QVERIFY(results.isEmpty());
}

void Tests::findFilesWithSomeWords1()
{
   qDebug() << "===== findFilesWithSomeWords1() =====";

   QString terms("aaaa bbbb cccc");

   FindResult expectedResult;
   expectedResult[0] << "aaaa bbbb cccc.txt";
   expectedResult[4] << "aaaa bbbb.txt";
   expectedResult[5] << "aaaa cccc.txt";
   expectedResult[6] << "cccc bbbb.nfo" << "bbbb cccc.nfo";
   expectedResult[7] << "aaaaaa bbbb.txt";
   expectedResult[9] << "cccc bbbbbb.txt";
   expectedResult[10] << "aaaaaa bbbbbb.txt";
   expectedResult[13] << "aaaa dddddd.txt";
   expectedResult[14] << "bbbb.txt" <<  "bbbb dddd.nfo";
   expectedResult[16] << "aaaaaa dddddd.txt";

   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, 65536);
   QVERIFY(!results.isEmpty());
   this->printSearch(terms, results.first());
   this->compareExpectedResult(results.first(), expectedResult);
}

void Tests::findFilesWithSomeWords2()
{
   qDebug() << "===== findFilesWithSomeWords2() =====";

   QString terms("aaaa bbbb cccc dddd");

   FindResult expectedResult;
   expectedResult[5] << "aaaa bbbb cccc.txt";
   expectedResult[21] << "aaaa bbbb.txt";
   expectedResult[22] << "aaaa cccc.txt";
   expectedResult[24] << "bbbb cccc.nfo" << "cccc bbbb.nfo";
   expectedResult[25] << "bbbb dddd.nfo";
   expectedResult[27] << "aaaaaa bbbb.txt";
   expectedResult[29] << "aaaa dddddd.txt";
   expectedResult[30] << "cccc bbbbbb.txt";
   expectedResult[33] << "aaaaaa bbbbbb.txt";
   expectedResult[35] << "aaaaaa dddddd.txt";
   expectedResult[40] << "bbbb.txt";
   expectedResult[42] << "dddd.txt";

   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, 65536);
   QVERIFY(!results.isEmpty());
   this->printSearch(terms, results.first());
   this->compareExpectedResult(results.first(), expectedResult);
}

void Tests::findFilesWithResultFragmentation()
{
   qDebug() << "===== findFilesWithResultFragmentation() =====";

   const int FRAGMENT_MAX_SIZE_BYTES = 400;

   QString terms("bbb");
   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, FRAGMENT_MAX_SIZE_BYTES);
   qDebug() << "Nb fragment: " << results.size();
   for (int i = 0; i < results.size(); i++)
   {
      qDebug() << "Fragment number " << i << ", size = " << results[i].ByteSizeLong();
      QVERIFY(results[i].ByteSizeLong() <= FRAGMENT_MAX_SIZE_BYTES);
      this->printSearch(terms, results[i]);
   }
}

void Tests::findFilesWithSomeWordsAndExtensions()
{
   qDebug() << "===== findFilesWithSomeWordsAndExtensions() =====";

   QString terms("aaaa bbbb cccc");

   FindResult expectedResult;
   expectedResult[0] << "aaaa bbbb cccc.txt";
   expectedResult[4] << "aaaa bbbb.txt";
   expectedResult[5] << "aaaa cccc.txt";
   expectedResult[7] << "aaaaaa bbbb.txt";
   expectedResult[9] << "cccc bbbbbb.txt";
   expectedResult[10] << "aaaaaa bbbbbb.txt";
   expectedResult[13] << "aaaa dddddd.txt";
   expectedResult[14] << "bbbb.txt";
   expectedResult[16] << "aaaaaa dddddd.txt";

   for (const QString& extension : QStringList { "txt", "TXT", "Txt" })
   {
      QList<Protos::Common::FindResult> results =
         this->fileManager->find(
            terms,
            QList<QString> { extension },
            0,
            std::numeric_limits<qint64>::max(),
            Protos::Common::FindPattern::FILE_DIR,
            10000,
            65536,
            true
         );
      QVERIFY(!results.isEmpty());
      this->printSearch(terms, results.first());
      this->compareExpectedResult(results.first(), expectedResult);
   }
}

void Tests::findFilesWithSomeWordsAndExtensionsAndSizeRange()
{
   qDebug() << "===== findFilesWithSomeWordsAndSizeRange() =====";

   QString terms("aaaa bbbb cccc");

   // Files with size too high or too low are commented.
   FindResult expectedResult;
   // expectedResult[0] << "aaaa bbbb cccc.txt"; // 18
   //expectedResult[4] << "aaaa bbbb.txt"; // 13
   //expectedResult[5] << "aaaa cccc.txt"; // 13
   expectedResult[7] << "aaaaaa bbbb.txt";  // 15
   expectedResult[9] << "cccc bbbbbb.txt"; // 15
   expectedResult[10] << "aaaaaa bbbbbb.txt"; // 17
   expectedResult[13] << "aaaa dddddd.txt"; // 15
   //expectedResult[14] << "bbbb.txt"; // 8
   expectedResult[16] << "aaaaaa dddddd.txt"; // 17

   for (const QString& extension : QStringList { "txt", "TXT", "Txt" })
   {
      QList<Protos::Common::FindResult> results =
         this->fileManager->find(
            terms,
            QList<QString> { extension },
            15, // >= 15 B.
            17, // <= 16 B.
            Protos::Common::FindPattern::FILE_DIR,
            10000,
            65536,
            true
         );
      QVERIFY(!results.isEmpty());
      this->printSearch(terms, results.first());
      this->compareExpectedResult(results.first(), expectedResult);
   }
}

void Tests::findFilesByExtensions()
{
   qDebug() << "===== findFilesByExtensions() =====";

   FindResult expectedResult;
   expectedResult[0] << "bbbb cccc.nfo" << "cccc bbbb.nfo" << "bbbb dddd.nfo";

   for (const QString& extension : QStringList { "nfo", "NFO", "Nfo" })
   {
      QList<Protos::Common::FindResult> results =
         this->fileManager->find(
            "",
            QList<QString> { extension },
            0,
            std::numeric_limits<qint64>::max(),
            Protos::Common::FindPattern::FILE_DIR,
            10000,
            65536,
            true
         );
      QVERIFY(!results.isEmpty());
      this->compareExpectedResult(results.first(), expectedResult);
   }
}

void Tests::findFilesByExtensionsAndSizeRange()
{
   qDebug() << "===== findFilesByExtensionsAndSizeRange() =====";

   FindResult expectedResult;
   expectedResult[0] << "aaaa bbbb.txt" << "aaaa cccc.txt" << "remoteFile.txt";

   for (const QString& extension : QStringList { "txt", "TXT", "Txt" })
   {
      QList<Protos::Common::FindResult> results =
         this->fileManager->find(
            "",
            QList<QString> { extension },
            12,
            13,
            Protos::Common::FindPattern::FILE_DIR,
            10000,
            65536,
            true
         );
      QVERIFY(!results.isEmpty());
      this->compareExpectedResult(results.first(), expectedResult);
   }
}

void Tests::findFilesBySizeRange()
{
   qDebug() << "===== findFilesBySizeRange() =====";

   FindResult expectedResult;
   expectedResult[0] <<
      "aaaaaa bbbb.txt" << // 15.
      "cccc bbbbbb.txt" << // 15.
      "aaaa dddddd.txt" << // 15.
      "aaaaaa bbbbbb.txt" << // 17
      "aaaaaa dddddd.txt" << // 17.
      "shared file.txt"; // 15

   QList<Protos::Common::FindResult> results =
      this->fileManager->find(
         "",
         QList<QString>(),
         15,
         17,
         Protos::Common::FindPattern::FILE_DIR,
         10000,
         65536,
         true
      );
   QVERIFY(!results.isEmpty());
   this->printSearch("", results.first());
   this->compareExpectedResult(results.first(), expectedResult);
}

void Tests::findSharedEntry()
{
   qDebug() << "===== findSharedEntry() =====";

   FindResult expectedResult;
   expectedResult[0] << "sharedDirs";

   QList<Protos::Common::FindResult> results =
      this->fileManager->find(
         "sharedDirs",
         QList<QString>(),
         0,
         std::numeric_limits<qint64>::max(),
         Protos::Common::FindPattern::FILE_DIR,
         10000,
         65536,
         true
      );

   QVERIFY(!results.isEmpty());
   this->printSearch("", results.first());
   this->compareExpectedResult(results.first(), expectedResult);
}

void Tests::findSharedEntryAfterRename()
{
   qDebug() << "===== findSharedEntryAfterRename() =====";

   this->sharedPaths.last().name = "sharedRenamed";
   this->fileManager->setSharedPaths(this->sharedPaths);

   // The previous name shouldn't be found.
   {
      QList<Protos::Common::FindResult> results =
         this->fileManager->find(
            "sharedDirs",
            QList<QString>(),
            0,
            std::numeric_limits<qint64>::max(),
            Protos::Common::FindPattern::FILE_DIR,
            10000,
            65536,
            true
         );

      QVERIFY(results.isEmpty());
   }

   FindResult expectedResult;
   expectedResult[0] << "sharedRenamed";

   QList<Protos::Common::FindResult> results =
      this->fileManager->find(
         "sharedRenamed",
         QList<QString>(),
         0,
         std::numeric_limits<qint64>::max(),
         Protos::Common::FindPattern::FILE_DIR,
         10000,
         65536,
         true
      );

   QVERIFY(!results.isEmpty());
   this->printSearch("", results.first());
   this->compareExpectedResult(results.first(), expectedResult);
}

void Tests::haveChunks()
{
   qDebug() << "===== haveChunks() =====";

   QList<Common::Hash> hashes;
   hashes
      << Common::Hash::fromStr("1cb04df35d745c1a5f3e514b5f09c7df5c0d0b8b43e6a3351ab85761").value() // "/sharedDirs/share3/aaaa bbbb cccc.txt"
      << Common::Hash::fromStr("7b6f7f3309179b97b88de3c178274b7e38343267bcdfe653c819593e").value() // "/sharedDirs/share1/v.txt"
      << Common::Hash::fromStr("6103413cbd2330b103bee4cdf16e18e1e19a9f00365d153ec8c61486").value() // "/sharedDirs/share1/y.txt" (deleted)
      << Common::Hash::fromStr("c0a43102ae34f72965ef08f23fd045a44b8ac29cd20031f5c90d9b79").value(); // Random hash

   QBitArray expectedResult(hashes.size());
   expectedResult[0] = true;
   expectedResult[1] = true;
   expectedResult[2] = false;
   expectedResult[3] = false;

   QBitArray result = this->fileManager->haveChunks(hashes);
   QCOMPARE(result.size(), hashes.size());

   for (int i = 0; i < result.size(); i++)
   {
      QVERIFY(result[i] == expectedResult[i]);
      qDebug() << hashes[i].toStr() << ":" << (result[i] ? "Yes" : "No");
   }
}

void Tests::printAmount()
{
   qDebug() << "===== printAmount() =====";

   auto amount = this->fileManager->getAmount();

   qDebug() << "Sharing amount: " << amount << " bytes (" << Common::Global::formatByteSize(amount) << ")";
   qDebug().noquote() << this->fileManager->getCacheTree_debug();

   QCOMPARE(amount, 268435691);
}

void Tests::rmSharedDirectory()
{
   qDebug() << "===== rmSharedDirectory() =====";

   this->sharedPaths.clear();
   this->fileManager->setSharedPaths(this->sharedPaths);

   auto amount = this->fileManager->getAmount();
   QCOMPARE(amount, 0);
}

/**
  * The following case tests the Bloom filter performance
  * used in the class 'Chunks'.
  * The bloom filter can be enable in "Chunks.h".
  */
#include <priv/ChunkIndex/Chunks.h>
#include <priv/Cache/Chunk.h>
#include <priv/Cache/Directory.h>
#include <priv/Cache/File.h>
void Tests::chunksPerformance()
{
   QSKIP("TODO: Move this to a benchmark test case");
   qDebug() << "===== chunksPerformance() =====";

   const int HASH_POOL_SIZE = 500000;
   const int NB_HASHES_TO_CHECK = 50000000;

   Chunks chunks;

   for (int i = 0; i < HASH_POOL_SIZE; i++)
   {
      QSharedPointer<Chunk> chunk(new Chunk(nullptr, 0, 0));
      chunk->setHash(Common::Hash::rand());
      chunks.add(chunk);
   }

   QList<Common::Hash> hashes;
   const int nbHashes = 100;
   for (int i = 0; i < nbHashes; i++)
      hashes << Common::Hash::rand();

   QElapsedTimer timer;
   timer.start();

   for (int i = 0; i < NB_HASHES_TO_CHECK; i++)
   {
      if(chunks.contains(hashes[i % nbHashes]))
         QFAIL("chunks cannot contains a random chunk");
   }

   qDebug() <<
      "Time to check if" << NB_HASHES_TO_CHECK <<
      "hashes exist among a pool of" << HASH_POOL_SIZE <<
      "hashes:" << timer.elapsed() << "ms";
}

#include <priv/ExtensionIndex.h>
void Tests::extensionIndexAddItem()
{
   QList<QString> mp3s { "file1.mp3", "file2.MP3" };

   ExtensionIndex<QString> index;
   index.addItem(mp3s[0].right(3), mp3s[0]);
   index.addItem(mp3s[1].right(3), mp3s[1]);
   index.addItem("jpg", "file3.jpg");

   for (int i = 0; i < 2; i++)
   {
      QList<QString> result = index.search(mp3s[i].right(3));
      QVERIFY(result.count() == 2);
      QVERIFY(mp3s.contains(result[0]));
      QVERIFY(mp3s.contains(result[1]));
   }

   QList<QString> result = index.search("jpg");
   QVERIFY(result.count() == 1);
   QVERIFY(result[0] == "file3.jpg");
}

void Tests::extensionIndexRmItem()
{
   QList<QString> mp3s { "file1.mp3", "file2.MP3" };

   ExtensionIndex<QString> index;
   index.addItem(mp3s[0].right(3), mp3s[0]);
   index.addItem(mp3s[1].right(3), mp3s[1]);
   index.addItem("jpg", "file3.jpg");

   index.rmItem(mp3s[0].right(3), mp3s[0]);
   index.rmItem("jpg", "file3.jpg");

   {
      QList<QString> result = index.search(mp3s[1].right(3));
      QVERIFY(result.count() == 1);
      QVERIFY(result[0] == "file2.MP3");
   }

   {
      QList<QString> result = index.search("jpg");
      QVERIFY(result.count() == 0);
   }
}

void Tests::extensionIndexChangeItem()
{
   QList<QString> mp3s { "file1.mp3", "file2.MP3" };

   ExtensionIndex<QString> index;
   index.addItem(mp3s[0].right(3), mp3s[0]);
   index.addItem(mp3s[1].right(3), mp3s[1]);

   index.changeItem("mp3", "JPG", mp3s[1]);

   {
      QList<QString> result = index.search("mp3");
      QVERIFY(result.count() == 1);
      QVERIFY(result[0] == "file1.mp3");
   }

   {
      QList<QString> result = index.search("jpg");
      QVERIFY(result.count() == 1);
      QVERIFY(result[0] == "file2.MP3");
   }
}

void Tests::extensionIndexSearchWithOneExtension()
{
   QList<QString> mp3s { "file1.mp3", "file2.MP3" };

   ExtensionIndex<QString> index;
   index.addItem(mp3s[0].right(3), mp3s[0]);
   index.addItem(mp3s[1].right(3), mp3s[1]);
   index.addItem("jpg", "file3.jpg");

   // When we search an existing extension by using a precise predicate we should get one result.
   {
      QList<QString> result = index.search("jpg", 1, [](const QString& item){ return item == "file3.jpg"; });
      QVERIFY(result.count() == 1);
      QVERIFY(result[0] == "file3.jpg");
   }

   // When we search by using a null string as the extension we should get nothing.
   {
      QList<QString> result = index.search(QString());
      QVERIFY(result.count() == 0);
   }

   // When we search an existing extension but with a always wrong predicate we should get nothing.
   {
      QList<QString> result = index.search("jpg", 1, [](const QString& item){ return item == "file3.mp3"; });
      QVERIFY(result.count() == 0);
   }

   // We search an existing extension among some possibilities but we keep the only one matching the given predicate.
   // #1.
   {
      QList<QString> result = index.search("MP3", 1, [](const QString& item){ return item == "file1.mp3"; });
      QVERIFY(result.count() == 1);
      QVERIFY(result[0] == "file1.mp3");
   }
   // #2.
   {
      QList<QString> result = index.search("MP3", 1, [](const QString& item){ return item == "file2.MP3"; });
      QVERIFY(result.count() == 1);
      QVERIFY(result[0] == "file2.MP3");
   }
}

void Tests::extensionIndexSearchWithSomeExtensions()
{
   QList<QString> mp3s { "file1.mp3", "file2.MP3" };

   ExtensionIndex<QString> index;
   index.addItem(mp3s[0].right(3), mp3s[0]);
   index.addItem(mp3s[1].right(3), mp3s[1]);
   index.addItem("jpg", "file3.jpg");

   {
      QList<QString> result = index.search(QList<QString>{ "jpg", "mp3", "png"}, 10);
      QVERIFY(result.count() == 3);
      QVERIFY(result.contains(mp3s[0]));
      QVERIFY(result.contains(mp3s[1]));
      QVERIFY(result.contains("file3.jpg"));
   }

   {
      QList<QString> result = index.search(QList<QString>{ "png", "html"}, 10);
      QVERIFY(result.count() == 0);
   }
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";

   // This call is only used to stop the fileUpdater and wait for it to finish.
   // It's should not be used in a normal code.
   this->fileManager.clear();

   QTest::qWait(200);
}

void Tests::createInitialFiles()
{
   this->deleteAllFiles();

   QVERIFY(Common::Global::createFile("sharedDirs/share1/subdir/o.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share1/subdir/p.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share1/another subdir/q.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share1/empty subdir/"));
   QVERIFY(Common::Global::createFile("sharedDirs/share1/r.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share1/s.txt"));

   QVERIFY(Common::Global::createFile("sharedDirs/share2/t.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share2/u.txt"));

   QVERIFY(Common::Global::createFile("shared file.txt"));

   // "share3" is dedicated to the search feature.
   QVERIFY(Common::Global::createFile("sharedDirs/share3/aaaa bbbb cccc.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/aaaa bbbb.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/aaaaaa bbbb.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/aaaaaa bbbbbb.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/aaaa cccc.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/aaaa dddddd.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/aaaaaa dddddd.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/bbbb cccc.nfo"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/cccc bbbb.nfo"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/bbbb dddd.nfo"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/bbbb.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/cccc bbbbbb.txt"));
   QVERIFY(Common::Global::createFile("sharedDirs/share3/dddd.txt"));

   QVERIFY(Common::Global::createFile("incoming/"));
}

void Tests::deleteAllFiles()
{
   Common::Global::recursiveDeleteDirectory("sharedDirs");
   Common::Global::recursiveDeleteDirectory("incoming");
}

void Tests::printSearch(const QString& terms, const Protos::Common::FindResult& result)
{
   qDebug() << "Search: " << terms;
   for (int i = 0; i < result.entries_size(); i++)
   {
      auto name = result.entries(i).entry().name();
      if (name.empty())
         name = result.entries(i).entry().shared_entry().shared_name();

      qDebug() << "[" << result.entries(i).level() << "] " << name;
   }
}

void Tests::compareExpectedResult(const Protos::Common::FindResult& result, const FindResult& expectedResult)
{
   int nbResult = 0;
   for (const auto& entries : expectedResult)
      nbResult += entries.size();

   QCOMPARE(result.entries_size(), nbResult);

   for (int i = 0; i < result.entries_size(); i++)
   {
      auto level = result.entries(i).level();
      QVERIFY(expectedResult.contains(level));

      auto name = result.entries(i).entry().name();
      if (name.empty())
         name = result.entries(i).entry().shared_entry().shared_name();

      QVERIFY(expectedResult[level].contains(name));
   }
}

void Tests::compareStrRegexp(const QString& regexp, const QString& str)
{
   QRegularExpression expected(regexp);
   auto match = expected.match(str);
   if (!match.hasMatch())
   {
      int l = match.capturedLength();
      QByteArray message =
         QString("This string doesn't match the expected regular expression from character %1: \n%2")
            .arg(l).arg(str).toUtf8();
      QFAIL(message.data());
   }
}
