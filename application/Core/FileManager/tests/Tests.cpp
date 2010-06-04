#include <Tests.h>
using namespace FM;

#include <QtDebug>
#include <QRegExp>
#include <QFile>
#include <QTextStream>
#include <QDataStream>
#include <QStringList>
#include <QDirIterator>

#include <IChunk.h>
#include <Common/LogManager/Builder.h>
#include <Common/PersistantData.h>
#include <Exceptions.h>
#include <priv/Constants.h>

Tests::Tests()
{
}

#include <iostream>
using namespace std;

void Tests::initTestCase()
{
   LM::Builder::initMsgHandler();
   qDebug() << "===== initTestCase() =====";

   Common::PersistantData::rmValue(FILE_CACHE); // Reset the stored cache.
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

   Tests::createFile("sharedDirs/x.txt");
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

   Tests::createFile("sharedDirs/share1/v.txt");
   QTest::qSleep(100);
}

void Tests::createABigFile()
{
   return;
   qDebug() << "===== createABigFile() =====";

   QFile file("sharedDirs/big.bin");
   file.open(QIODevice::WriteOnly);
   file.resize(128 * 1024 * 1024); // 128Mo
   QTest::qSleep(1000);
}

void Tests::modifyABigFile()
{
   return;
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
   return;
   qDebug() << "===== removeABigFile() =====";

   while(!QFile("sharedDirs/big.bin").remove())
      QTest::qSleep(100);
   QTest::qSleep(100);
}

void Tests::createADirectory()
{
   qDebug() << "===== createADirectory() =====";

   Tests::createFile("sharedDirs/a/");
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

   Tests::recursiveDeleteDirectory("sharedDirs/share1/share2");
   QTest::qSleep(100);
}

void Tests::browseSomedirectories()
{
   qDebug() << "===== browseSomedirectories() =====";

   // TODO : active the regexp comparison.

   // Get the shared directories.
   Protos::Core::GetEntriesResult entries1 = this->fileManager->getEntries();
   QString entries1Str = QString::fromStdString(entries1.DebugString());
   /*Tests::compareStrRegexp(
      "dir\\s*\\{\n\\s*shared_dir\\s*\\{\n\\s*id\\s*\\{\n\\s*hash:\\s*\".+\"\n\\s*\\}\n\\s*\\}\n\\s*path:\\s*\"\"\n\\s*name:\\s*\"sharedDirs\"\n\\s*size:\\s*\\d+\n\\}\ndir\\s*\\{\n\\s*shared_dir\\s*\\{\n\\s*id\\s*\\{\n\\s*hash:\\s*\".+\"\n\\s*\\}\n\\s*\\}\n\\s*path:\\s*\"\"\n\\s*name:\\s*\"incoming\"\n\\s*size:\\s*\\d+\n\\}.*",
      entries1Str
   );*/
   qDebug() << entries1Str;

   // Ask for the files and directories of the first shared directory
   Protos::Core::GetEntriesResult entries2 = this->fileManager->getEntries(entries1.dir(0));
   qDebug() << QString::fromStdString(entries2.DebugString());

   // Ask for the files and directores of the first directory of the first shared directory
   Protos::Core::GetEntriesResult entries3 = this->fileManager->getEntries(entries2.dir(0));
   qDebug() << QString::fromStdString(entries3.DebugString());
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

   Protos::Common::FindResult result = this->fileManager->find(terms);
   this->printSearch(terms, result);
   this->compareExpectedResult(result, expectedLevelResult, expectedFileResult);
}


void Tests::findUnexistingFilesWithOneWord()
{
   qDebug() << "===== findUnexistingFilesWithOneWord() =====";

   QString terms("mmmm");
   quint32 expectedLevelResult[] = {};
   QList<QString> expectedFileResult[] = {};

   Protos::Common::FindResult result = this->fileManager->find(terms);
   this->printSearch(terms, result);
   this->compareExpectedResult(result, expectedLevelResult, expectedFileResult);

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

   Protos::Common::FindResult result = this->fileManager->find(terms);
   this->printSearch(terms, result);
   this->compareExpectedResult(result, expectedLevelResult, expectedFileResult);
}

void Tests::printAmount()
{
   qDebug() << "Sharing amount : " << this->fileManager->getAmount() << " bytes";
}

void Tests::rmSharedDirectory()
{
   qDebug() << "===== rmSharedDirectory() =====";

   this->sharedDirsReadOnly.clear();
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
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

   Tests::createFile("sharedDirs/share1/subdir/o.txt");
   Tests::createFile("sharedDirs/share1/subdir/p.txt");
   Tests::createFile("sharedDirs/share1/another subdir/q.txt");
   Tests::createFile("sharedDirs/share1/empty subdir/");
   Tests::createFile("sharedDirs/share1/r.txt");
   Tests::createFile("sharedDirs/share1/s.txt");

   Tests::createFile("sharedDirs/share2/t.txt");
   Tests::createFile("sharedDirs/share2/u.txt");

   // "share3" is dedicated to the search feature.
   Tests::createFile("sharedDirs/share3/aaaa bbbb cccc.txt");
   Tests::createFile("sharedDirs/share3/aaaa bbbb.txt");
   Tests::createFile("sharedDirs/share3/aaaa cccc.txt");
   Tests::createFile("sharedDirs/share3/aaaa dddddd.txt");
   Tests::createFile("sharedDirs/share3/bbbb cccc.txt");
   Tests::createFile("sharedDirs/share3/bbbb dddd.txt");
   Tests::createFile("sharedDirs/share3/bbbb.txt");
   Tests::createFile("sharedDirs/share3/cccc bbbbbb.txt");
   Tests::createFile("sharedDirs/share3/dddd.txt");

   Tests::createFile("incoming/");
}

void Tests::deleteAllFiles()
{
   Tests::recursiveDeleteDirectory("sharedDirs");
   Tests::recursiveDeleteDirectory("incoming");
}

void Tests::search()
{
   /**
     * Execute many times the search in parallel
     * with the indexing.
     */
   /*
   for (int i = 0; i < 20; i++)
   {
      this->printAmount();
      this->doASearch(false);
   }

   QTest::qSleep(500);

   this->doASearch(true);
   this->printAmount();

   this->sharedDirsReadOnly << QDir::currentPath().append("/sharedDirs");
   this->fileManager->setSharedDirsReadWrite(this->sharedDirsReadOnly);

   QTest::qSleep(500);

   QString terms("xxxx");
   Protos::Common::FindResult result = this->fileManager->find(terms);
   this->printSearch(terms, result);*/
}

void Tests::printSearch(const QString& terms, const Protos::Common::FindResult& result)
{
   qDebug() << "Search : " << terms;
   for (int i = 0; i < result.file_size(); i++)
      qDebug() << "[" << result.file(i).level() << "] " << result.file(i).file().file().name().data();
}

void Tests::compareExpectedResult(const Protos::Common::FindResult& result, quint32 expectedLevelResult[], QList<QString> expectedFileResult[])
{
   for (int i = 0; i < result.file_size(); i++)
   {
      QVERIFY(result.file(i).level() == expectedLevelResult[i]);
      QVERIFY(expectedFileResult[result.file(i).level()].contains(QString(result.file(i).file().file().name().data())));
   }
}

/**
  * Create a file and its parent directories if needed.
  */
void Tests::createFile(const QString& path)
{
   QFileInfo fileInfo(path);
   QDir::current().mkpath(fileInfo.path());
   if (fileInfo.fileName().isEmpty())
      return;

   QFile file(path);
   file.open(QIODevice::WriteOnly);
   QTextStream stream(&file);
   stream << fileInfo.fileName();
}

void Tests::recursiveDeleteDirectory(const QString& dir)
{
   for (QDirIterator i(dir, QDir::Files, QDirIterator::Subdirectories); i.hasNext();)
      QFile(i.next()).remove();

   for (QDirIterator i(dir, QDir::AllDirs, QDirIterator::Subdirectories); i.hasNext();)
      QDir::current().rmpath(i.next());
}

void Tests::compareStrRegexp(const QString& regexp, const QString& str)
{
   QRegExp expected(regexp);
   if (!expected.exactMatch(str))
   {
      int l = expected.matchedLength();
      QFAIL(QString("This string doesn't match the expected regular expression from character %1 : \n%2").arg(l).arg(str).toStdString().data());
   }
}

