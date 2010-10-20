#include <Tests.h>
using namespace FM;

#include <string>
using namespace std;

#include <QtDebug>
#include <QRegExp>
#include <QFile>
#include <QTextStream>
#include <QDataStream>
#include <QStringList>
#include <QDirIterator>

#include <Protos/core_settings.pb.h>

#include <Common/LogManager/Builder.h>
#include <Common/PersistantData.h>
#include <Common/Constants.h>
#include <Common/Global.h>
#include <Common/ProtoHelper.h>
#include <Common/Settings.h>

#include <IChunk.h>
#include <IGetHashesResult.h>
#include <Exceptions.h>
#include <priv/Constants.h>

#include <StressTest.h>
#include <HashesReceiver.h>

Tests::Tests()
{
}

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();
   qDebug() << "===== initTestCase() =====";

   Common::PersistantData::rmValue(Common::FILE_CACHE); // Reset the stored cache.

   SETTINGS.setFilename("core_settings.txt");
   SETTINGS.setSettingsMessage(new Protos::Core::Settings());
}

void Tests::createFileManager()
{
   qDebug() << "===== createFileManager() =====";

   this->createInitialFiles();
   this->fileManager = Builder::newFileManager();
}

void Tests::addASharedDirectory()
{
   qDebug() << "===== addASharedDirectory() =====";

   this->sharedDirsReadOnly << QDir::currentPath().append("/sharedDirs/share1");
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
   QStringList paths = this->fileManager->getSharedDirsReadOnly();
   QVERIFY(paths.size() == 1);
   QCOMPARE(paths.at(0), QDir::cleanPath(this->sharedDirsReadOnly.at(0)));
   QVERIFY(this->fileManager->getSharedDirsReadWrite().size() == 0);
}

void Tests::addAnAlreadySharedDirectory()
{
   qDebug() << "===== addAnAlreadySharedDirectory() =====";

   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
   QStringList paths = this->fileManager->getSharedDirsReadOnly();
   QVERIFY(paths.size() == 1);
   QCOMPARE(paths.at(0), QDir::cleanPath(this->sharedDirsReadOnly.at(0)));
   QVERIFY(this->fileManager->getSharedDirsReadWrite().size() == 0);
}

void Tests::addInexistingSharedDirectory()
{
   qDebug() << "===== addInexistingSharedDirectory() =====";

   this->sharedDirsReadOnly << QDir::currentPath().append("/this_is_spartaaaaaa"); // This directory doesn't exit.
   try
   {
      this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
      QFAIL("An exception must be thrown");
   }
   catch (DirsNotFoundException& e)
   {
      QVERIFY(e.paths.size() == 1);
      QCOMPARE(e.paths.at(0), QDir::cleanPath(this->sharedDirsReadOnly.last()));
      qDebug() << "This directory hasn't been found : " << e.paths.at(0) << " (Exception thrown)";
   }
   this->sharedDirsReadOnly.removeLast();
}

void Tests::addSubSharedDirectories()
{
   qDebug() << "===== addSubSharedDirectories() =====";

   this->sharedDirsReadOnly << QDir::currentPath().append("/sharedDirs/share1/subdir");
   this->sharedDirsReadOnly << QDir::currentPath().append("/sharedDirs/share1/another subdir");
   try
   {
      this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
      QFAIL("An exception must be thrown");
   }
   catch(SuperDirectoryExistsException& e)
   {
      QCOMPARE(e.superDirectory, QDir::cleanPath(this->sharedDirsReadOnly.at(0)));
      QCOMPARE(e.subDirectory, QDir::cleanPath(this->sharedDirsReadOnly.at(this->sharedDirsReadOnly.size()-2)));

      qDebug() << "There is already a super directory : " << e.superDirectory <<
            " for this directory : " << e.subDirectory;
   }
   this->sharedDirsReadOnly.removeLast();
   this->sharedDirsReadOnly.removeLast();
}

void Tests::addSuperSharedDirectoriesWithDifferentRights()
{
   qDebug() << "===== addSuperSharedDirectoriesWithDifferentRights() =====";

   QStringList sharedWriteDirs;
   sharedWriteDirs << QDir::currentPath().append("/sharedDirs");

   try
   {
      this->fileManager->setSharedDirsReadWrite(sharedWriteDirs);
      QFAIL("An exception must be thrown");
   }
   catch(SubDirectoriesWithDifferentRightsExistsException& e)
   {
      qDebug() << "This directory : " << e.superDirectory <<
            " has sub directories with different rights : " << e.subDirectories;
   }
}

/**
  * The subs directories of each subdirectory must be merged into the super directory.
  */
void Tests::addSuperSharedDirectoriesWithSameRights()
{
   qDebug() << "===== addSuperSharedDirectoriesWithSameRights() =====";

   this->sharedDirsReadOnly << QDir::currentPath().append("/sharedDirs");
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);

   QTest::qSleep(100);

   /*
   qsrand(QTime(0,0,0).secsTo(QTime::currentTime()));
   QTest::qSleep(qrand() % 10 + 1);*/
}

void Tests::addASharedDirectoryReadWrite()
{
   qDebug() << "===== addASharedDirectoryReadWrite() =====";

   QStringList sharedWriteDirs;
   sharedWriteDirs << QDir::currentPath().append("/incoming");
   this->fileManager->setSharedDirsReadWrite(sharedWriteDirs);
}

void Tests::createAFile()
{
   qDebug() << "===== createAFile() =====";

   Common::Global::createFile("sharedDirs/x.txt");
   QTest::qSleep(100);
}

void Tests::moveAFile()
{
   qDebug() << "===== moveAFile() =====";

   QDir::current().rename("sharedDirs/x.txt", "sharedDirs/share1/x.txt");
   QTest::qSleep(100);
}

void Tests::renameAFile()
{
   qDebug() << "===== renameAFile() =====";

   QDir::current().rename("sharedDirs/share1/x.txt", "sharedDirs/share1/y.txt");
   QTest::qSleep(100);
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
   QTest::qSleep(100);
}

void Tests::removeAFile()
{
   qDebug() << "===== removeAFile() =====";

   QFile("sharedDirs/share1/y.txt").remove();
   QTest::qSleep(100);
}

void Tests::createASubFile()
{
   qDebug() << "===== createASubFile() =====";

   Common::Global::createFile("sharedDirs/share1/v.txt");
   QTest::qSleep(100);
}

void Tests::createABigFile()
{
   qDebug() << "===== createABigFile() =====";

   QFile file("sharedDirs/big.bin");
   file.open(QIODevice::WriteOnly);
   file.resize(128 * 1024 * 1024); // 128Mo
   QTest::qSleep(1000);
}

void Tests::modifyABigFile()
{
   qDebug() << "===== modifyABigFile() =====";

   {
      QFile file("sharedDirs/big.bin");
      file.open(QIODevice::ReadWrite);
      QDataStream stream(&file);
      stream.skipRawData(32 * 1024 * 1024 - 3);
      QByteArray data("XXXXXX");
      stream.writeRawData(data.constData(), data.size());
   }

   QTest::qSleep(1000);
}

void Tests::removeABigFile()
{
   qDebug() << "===== removeABigFile() =====";

   while(!QFile("sharedDirs/big.bin").remove())
      QTest::qSleep(100);
   QTest::qSleep(100);
}

void Tests::createADirectory()
{
   qDebug() << "===== createADirectory() =====";

   Common::Global::createFile("sharedDirs/a/");
   QTest::qSleep(100);
}

void Tests::renameADirectory()
{
   qDebug() << "===== renameADirectory() =====";

   QDir("sharedDirs").rename("a", "b");
   QTest::qSleep(100);
}

void Tests::moveAnEmptyDirectory()
{
   qDebug() << "===== moveAnEmptyDirectory() =====";

   QDir("sharedDirs").rename("b", "share1/b");
   QTest::qSleep(100);
}

void Tests::moveADirectoryContainingFiles()
{
   qDebug() << "===== moveADirectoryContainingFiles() =====";

   QDir("sharedDirs").rename("share2", "share1/share2");
   QTest::qSleep(100);
}

void Tests::removeADirectory()
{
   qDebug() << "===== removeADirectory() =====";

   Common::Global::recursiveDeleteDirectory("sharedDirs/share1/share2");
   QTest::qSleep(100);
}

void Tests::createAnEmptyFile()
{
   qDebug() << "===== createAnEmptyFile() =====";

   Protos::Common::Entry remoteEntry;
   remoteEntry.set_path("/remoteShare1/");
   remoteEntry.set_name("remoteFile.txt");
   remoteEntry.set_size(1 * 1024 * 1024); // 1Mo.

   try
   {
      QList< QSharedPointer<IChunk> > chunks = this->fileManager->newFile(remoteEntry);
      for (int i = 0; i < chunks.size(); i++)
         QVERIFY(chunks[i]->getHash().isNull());
   }
   catch(NoReadWriteSharedDirectoryException&)
   {
      QFAIL("NoReadWriteSharedDirectoryException");
   }
   catch(InsufficientStorageSpaceException&)
   {
      QFAIL("InsufficientStorageSpaceException");
   }
   catch(FilePhysicallyAlreadyExistsException)
   {
      QFAIL("FilePhysicallyAlreadyExistsException");
   }
   catch(UnableToCreateNewFileException&)
   {
      QFAIL("UnableToCreateNewFileException");
   }
}

void Tests::getAExistingChunk()
{
   qDebug() << "===== getAExistingChunk() =====";

   QSharedPointer<IChunk> chunk = this->fileManager->getChunk(Common::Hash::fromStr("97d464813598e2e4299b5fe7db29aefffdf2641d"));
   if (chunk.isNull())
      QFAIL("Chunk not found");
   else
      qDebug() << "Chunk found : " << chunk->getHash().toStr();
}

void Tests::getAUnexistingChunk()
{
   qDebug() << "===== getAUnexistingChunk() =====";

   QSharedPointer<IChunk> chunk = this->fileManager->getChunk(Common::Hash::fromStr("47ddfe38b8c66c0f9d98b9d802f220c84b4b30d4"));
   if (chunk.isNull())
      qDebug() << "Chunk not found : ok";
   else
      QFAIL("No chunk must be found");
}

void Tests::getHashesFromAFileEntry1()
{
   qDebug() << "===== getHashesFromAFileEntry1() =====";

   // Find the id of the first shared directory.
   Protos::Common::Entries sharedDirs = this->fileManager->getEntries();
   const string sharedDirId = sharedDirs.entry(0).shared_dir().id().hash();

   Protos::Common::Entry entry;
   entry.set_path("/share1/");
   entry.set_name("v.txt");
   entry.mutable_shared_dir()->mutable_id()->set_hash(sharedDirId);
   QSharedPointer<IGetHashesResult> result = this->fileManager->getHashes(entry);

   HashesReceiver hashesReceiver;
   connect(result.data(), SIGNAL(nextHash(Common::Hash)), &hashesReceiver, SLOT(nextHash(Common::Hash)));

   Protos::Core::GetHashesResult res = result->start();

   QCOMPARE(res.status(), Protos::Core::GetHashesResult_Status_OK);
}

void Tests::getHashesFromAFileEntry2()
{
   qDebug() << "===== getHashesFromAFileEntry2() =====";

   {
      QFile file1("sharedDirs/big2.bin");
      file1.open(QIODevice::WriteOnly);

      QFile file2("sharedDirs/big3.bin");
      file2.open(QIODevice::WriteOnly);

      file1.resize(128 * 1024 * 1024); // 128Mo
      file2.resize(128 * 1024 * 1024); // 128Mo
   }
   QTest::qWait(2000); // Begin the computing of the big2.bin hashes.

   Protos::Common::Entries sharedDirs = this->fileManager->getEntries();
   const string sharedDirId = sharedDirs.entry(0).shared_dir().id().hash();

   Protos::Common::Entry entry;
   entry.set_path("/");
   entry.set_name("big3.bin");
   entry.mutable_shared_dir()->mutable_id()->set_hash(sharedDirId);
   QSharedPointer<IGetHashesResult> result = this->fileManager->getHashes(entry);

   HashesReceiver hashesReceiver;
   connect(result.data(), SIGNAL(nextHash(Common::Hash)), &hashesReceiver, SLOT(nextHash(Common::Hash)));
   Protos::Core::GetHashesResult res = result->start(); // Should stop the computing of 'big2.bin' and switch to 'big3.bin'.
   QCOMPARE(res.status(), Protos::Core::GetHashesResult_Status_OK);

   QTest::qWait(4000);
}

void Tests::browseSomedirectories()
{
   qDebug() << "===== browseSomedirectories() =====";

   // TODO : active the regexp comparison.

   // Get the shared directories.
   Protos::Common::Entries entries1 = this->fileManager->getEntries();
   QString entries1Str = Common::ProtoHelper::getDebugStr(entries1);
   /*Tests::compareStrRegexp(
      "dir\\s*\\{\n\\s*shared_dir\\s*\\{\n\\s*id\\s*\\{\n\\s*hash:\\s*\".+\"\n\\s*\\}\n\\s*\\}\n\\s*path:\\s*\"\"\n\\s*name:\\s*\"sharedDirs\"\n\\s*size:\\s*\\d+\n\\}\ndir\\s*\\{\n\\s*shared_dir\\s*\\{\n\\s*id\\s*\\{\n\\s*hash:\\s*\".+\"\n\\s*\\}\n\\s*\\}\n\\s*path:\\s*\"\"\n\\s*name:\\s*\"incoming\"\n\\s*size:\\s*\\d+\n\\}.*",
      entries1Str
   );*/
   QVERIFY(entries1Str != "");
   qDebug() << entries1Str;

   // Ask for the files and directories of the first shared directory
   Protos::Common::Entries entries2 = this->fileManager->getEntries(entries1.entry(0));
   qDebug() << Common::ProtoHelper::getDebugStr(entries2);

   // Ask for the files and directores of the first directory of the first shared directory
   Protos::Common::Entries entries3 = this->fileManager->getEntries(entries2.entry(0));
   qDebug() << Common::ProtoHelper::getDebugStr(entries3);
}

void Tests::findExistingFilesWithOneWord()
{
   qDebug() << "===== findExistingFilesWithOneWord() =====";

   QString terms("aaaa");
   quint32 expectedLevelResult[] = {
      0, 0, 0, 0
   };
   QList<QString> expectedFileResult[] = {
      QList<QString>() << "aaaa dddddd.txt" << "aaaa cccc.txt" << "aaaa bbbb.txt" << "aaaa bbbb cccc.txt"
   };

   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, 65536);
   QVERIFY(!results.isEmpty());
   this->printSearch(terms, results.first());
   this->compareExpectedResult(results.first(), expectedLevelResult, expectedFileResult);
}


void Tests::findUnexistingFilesWithOneWord()
{
   qDebug() << "===== findUnexistingFilesWithOneWord() =====";

   QString terms("mmmm");
   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, 65536);
   QVERIFY(results.isEmpty());
}

void Tests::findFilesWithSomeWords()
{
   qDebug() << "===== findFilesWithSomeWords() =====";

   QString terms("aaaa bbbb cccc");
   quint32 expectedLevelResult[] = {
      0, 1, 2, 3, 3, 4, 5, 5
   };
   QList<QString> expectedFileResult[] = {
      QList<QString>() << "aaaa bbbb cccc.txt",
      QList<QString>() << "aaaa bbbb.txt",
      QList<QString>() << "aaaa cccc.txt",
      QList<QString>() << "cccc bbbbbb.txt" << "bbbb cccc.txt",
      QList<QString>() << "aaaa dddddd.txt",
      QList<QString>() << "bbbb dddd.txt" << "bbbb.txt"
   };

   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, 65536);
   QVERIFY(!results.isEmpty());
   this->printSearch(terms, results.first());
   this->compareExpectedResult(results.first(), expectedLevelResult, expectedFileResult);
}

void Tests::findFilesWithResultFragmentation()
{
   qDebug() << "===== findFilesWithResultFragmentation() =====";

   const int FRAGMENT_MAX_SIZE = 120;

   QString terms("bbb");
   QList<Protos::Common::FindResult> results = this->fileManager->find(terms, 10000, FRAGMENT_MAX_SIZE);
   qDebug() << "Nb fragment : " << results.size();
   for (int i = 0; i < results.size(); i++)
   {
      QVERIFY(results[i].ByteSize() <= FRAGMENT_MAX_SIZE);
      qDebug() << "Fragment n°" << i << ", size = " << results[i].ByteSize();
      this->printSearch(terms, results[i]);
   }
}

void Tests::haveChunks()
{
   qDebug() << "===== haveChunks() =====";

   QList<Common::Hash> hashes;
   hashes
      << Common::Hash::fromStr("f6126deaa5e1d9692d54e3bef0507721372ee7f8") // "/sharedDirs/share3/aaaa bbbb cccc.txt"
      << Common::Hash::fromStr("4c24e58c47746ea04296df9342185d9b3a447899") // "/sharedDirs/share1/v.txt"
      << Common::Hash::fromStr("954531aef8ac193ad62f4de783da9d7e6ebd59dd") // "/sharedDirs/share1/y.txt" (deleted)
      << Common::Hash::fromStr("8374d82e993012aa23b293f319eef2c21d2da3b9"); // Random hash

   QBitArray excpectedResult(hashes.size());
   excpectedResult[0] = true;
   excpectedResult[1] = true;
   excpectedResult[2] = false;
   excpectedResult[3] = false;

   QBitArray result = this->fileManager->haveChunks(hashes);
   QVERIFY(result.size() == hashes.size());

   for (int i = 0; i < result.size(); i++)
   {
      QVERIFY(result[i] == excpectedResult[i]);
      qDebug() << hashes[i].toStr() << " : " << (result[i] ? "Yes" : "No");
   }
}

void Tests::printAmount()
{
   qDebug() << "===== printAmount() =====";

   qDebug() << "Sharing amount : " << this->fileManager->getAmount() << " bytes";
}

void Tests::rmSharedDirectory()
{
   qDebug() << "===== rmSharedDirectory() =====";

   this->sharedDirsReadOnly.clear();
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
}

/**
  * Some tasks will be performed concurrently.
  */
void Tests::stressTest()
{
   qDebug() << "===== stressTest() =====";

   return;
   Common::PersistantData::rmValue(Common::FILE_CACHE);
   StressTest test;
}

void Tests::cleanupTestCase()
{
   qDebug() << "===== cleanupTestCase() =====";
   // This call is only used to stop the fileUpdater and wait for it to finish.
   // It's should not be used in a normal code.
   //this->fileManager.clear();

   QTest::qSleep(200);
}

void Tests::createInitialFiles()
{
   this->deleteAllFiles();

   Common::Global::createFile("sharedDirs/share1/subdir/o.txt");
   Common::Global::createFile("sharedDirs/share1/subdir/p.txt");
   Common::Global::createFile("sharedDirs/share1/another subdir/q.txt");
   Common::Global::createFile("sharedDirs/share1/empty subdir/");
   Common::Global::createFile("sharedDirs/share1/r.txt");
   Common::Global::createFile("sharedDirs/share1/s.txt");

   Common::Global::createFile("sharedDirs/share2/t.txt");
   Common::Global::createFile("sharedDirs/share2/u.txt");

   // "share3" is dedicated to the search feature.
   Common::Global::createFile("sharedDirs/share3/aaaa bbbb cccc.txt");
   Common::Global::createFile("sharedDirs/share3/aaaa bbbb.txt");
   Common::Global::createFile("sharedDirs/share3/aaaa cccc.txt");
   Common::Global::createFile("sharedDirs/share3/aaaa dddddd.txt");
   Common::Global::createFile("sharedDirs/share3/bbbb cccc.txt");
   Common::Global::createFile("sharedDirs/share3/bbbb dddd.txt");
   Common::Global::createFile("sharedDirs/share3/bbbb.txt");
   Common::Global::createFile("sharedDirs/share3/cccc bbbbbb.txt");
   Common::Global::createFile("sharedDirs/share3/dddd.txt");

   Common::Global::createFile("incoming/");
}

void Tests::deleteAllFiles()
{
   Common::Global::recursiveDeleteDirectory("sharedDirs");
   Common::Global::recursiveDeleteDirectory("incoming");
}

void Tests::printSearch(const QString& terms, const Protos::Common::FindResult& result)
{
   qDebug() << "Search : " << terms;
   for (int i = 0; i < result.entry_size(); i++)
      qDebug() << "[" << result.entry(i).level() << "] " << Common::ProtoHelper::getStr(result.entry(i).entry(), &Protos::Common::Entry::name);
}

void Tests::compareExpectedResult(const Protos::Common::FindResult& result, quint32 expectedLevelResult[], QList<QString> expectedFileResult[])
{
   for (int i = 0; i < result.entry_size(); i++)
   {
      QVERIFY(result.entry(i).level() == expectedLevelResult[i]);
      QVERIFY(expectedFileResult[result.entry(i).level()].contains(Common::ProtoHelper::getStr(result.entry(i).entry(), &Protos::Common::Entry::name)));
   }
}

void Tests::compareStrRegexp(const QString& regexp, const QString& str)
{
   QRegExp expected(regexp);
   if (!expected.exactMatch(str))
   {
      int l = expected.matchedLength();
      QByteArray message = QString("This string doesn't match the expected regular expression from character %1 : \n%2").arg(l).arg(str).toUtf8();
      QFAIL(message.data());
   }
}

