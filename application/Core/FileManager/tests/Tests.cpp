#include <Tests.h>
using namespace FM;

#include <QtDebug>
#include <QFile>
#include <QTextStream>
#include <QStringList>
#include <QDirIterator>

#include <IChunk.h>
#include <Protos/common.pb.h>
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

   this->createFile("sharedDirs/k.txt");
   QTest::qSleep(100);
}

void Tests::moveAFile()
{
   qDebug() << "===== moveAFile() =====";

   QDir::current().rename("sharedDirs/k.txt", "sharedDirs/share1/k.txt");
   QTest::qSleep(100);
}

void Tests::renameAFile()
{
   qDebug() << "===== renameAFile() =====";

   QDir::current().rename("sharedDirs/share1/d.txt", "sharedDirs/share1/l.txt");
   QTest::qSleep(100);
}

void Tests::modifyAFile()
{
   qDebug() << "===== modifyAFile() =====";

   {
      QFile file("sharedDirs/share1/subdir/a.txt");
      file.open(QIODevice::Append);
      QTextStream stream(&file);
      stream << "test";
   }
   QTest::qSleep(100);
}

void Tests::removeAFile()
{
   qDebug() << "===== removeAFile() =====";

   QFile("sharedDirs/share1/subdir/a.txt").remove();
   QTest::qSleep(100);
}

void Tests::createABigFile()
{
   qDebug() << "===== createABigFile() =====";

   QFile file("sharedDirs/big.bin");
   file.open(QIODevice::WriteOnly);
   file.resize(1024 * 1024 * 1024); // 1Go
   QTest::qSleep(1000000);
}

void Tests::modifyABigFile()
{

}

void Tests::removeABigFile()
{

}

void Tests::createADirectory()
{

}

void Tests::moveADirectory()
{

}

void Tests::removeADirectory()
{

}

void Tests::browseAdirectory()
{
   return;

   Protos::Core::GetEntriesResult entries = this->fileManager->getEntries();
   qDebug() << QString::fromStdString(entries.DebugString());

   Protos::Core::GetEntriesResult entries2 = this->fileManager->getEntries(entries.dir(0));
   qDebug() << QString::fromStdString(entries2.DebugString());
}

void Tests::rmSharedDirectory()
{
   qDebug() << "===== rmSharedDirectory() =====";

   this->sharedDirsReadOnly.clear();
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
}

void Tests::cleanupTestCase()
{
   // This call is only used to stop the fileUpdater and wait for it to finish.
   // It's should not be used in a normal code.
   //this->fileManager.clear();

   QTest::qSleep(200);
}

void Tests::createInitialFiles()
{
   this->deleteAllFiles();

   this->createFile("sharedDirs/share1/subdir/a.txt");
   this->createFile("sharedDirs/share1/subdir/b.txt");
   this->createFile("sharedDirs/share1/another subdir/c.txt");
   this->createFile("sharedDirs/share1/empty subdir/");
   this->createFile("sharedDirs/share1/d.txt");
   this->createFile("sharedDirs/share1/e.txt");

   this->createFile("sharedDirs/share2/f.txt");
   this->createFile("sharedDirs/share2/g.txt");

   this->createFile("sharedDirs/share3/h.txt");
   this->createFile("sharedDirs/share3/i.txt");
   this->createFile("sharedDirs/share3/j.txt");

   this->createFile("incoming/");
}

void Tests::deleteAllFiles()
{
   foreach (QString dir, QStringList() << "sharedDirs" << "incoming")
   {
      for (QDirIterator i(dir, QDir::Files, QDirIterator::Subdirectories); i.hasNext();)
         QFile(i.next()).remove();

      for (QDirIterator i(dir, QDir::AllDirs, QDirIterator::Subdirectories); i.hasNext();)
         QDir::current().rmpath(i.next());
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

/*
   this->sharedDirs << QDir::currentPath().append("/asdasdasd"); // This directory doesn't exit.

   try
   {
      this->fileManager->setSharedDirsReadOnly(this->sharedDirs);
   }
   catch (DirsNotFoundException& e)
   {
      foreach (QString path, e.getPaths())
         qDebug() << "This directory hasn't been found : " << path << " (Exception thrown)";
   }

   this->sharedDirs.removeLast(); // Remove the nonexistent directory.
   */

void Tests::search()
{
   /**
     * Execute many times the search in parallel
     * with the indexing.
     */
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
   this->printSearch(terms, result);
}

void Tests::addSuperSharedDirectoriesAndMerge()
{
   /*
   QStringList dirs;
   dirs << QDir::currentPath().append("/../../aSharedDir/subdir") <<
         QDir::currentPath().append("/../../aSharedDir/anotherSubdir") <<
         QDir::currentPath().append("/../../aSharedDir");
   this->fileManager2->setSharedDirsReadOnly(dirs);*/
}

void Tests::doASearch(bool checkResult)
{
   QString terms("aaaa bbbb cccc");
   Protos::Common::FindResult result = this->fileManager->find(terms);
   this->printSearch(terms, result);

   if (checkResult)
   {
      quint32 levelResults[] = {
         0, 1, 2, 3, 3, 4, 5, 5
      };
      QList<QString> fileResults[] = {
         QList<QString>() << "aaaa bbbb cccc.txt",
         QList<QString>() << "aaaa bbbb.txt",
         QList<QString>() << "aaaa cccc.txt",
         QList<QString>() << "cccc bbbbbb.txt" << "bbbb cccc.txt",
         QList<QString>() << "aaaa dddddd.txt",
         QList<QString>() << "bbbb dddd.txt" << "bbbb.txt"
      };
      for (int i = 0; i < result.file_size(); i++)
      {
         QVERIFY(result.file(i).level() == levelResults[i]);
         QVERIFY(fileResults[result.file(i).level()].contains(QString(result.file(i).file().file().name().data())));
      }
   }
}

void Tests::printSearch(const QString& terms, const Protos::Common::FindResult& result)
{
   qDebug() << "Search : '" << terms << "'";
   for (int i = 0; i < result.file_size(); i++)
      qDebug() << "[" << result.file(i).level() << "] " << result.file(i).file().file().name().data();
}

void Tests::printAmount()
{
   qDebug() << "Sharing amount : " << this->fileManager->getAmount() << " bytes";
}

