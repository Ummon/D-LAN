#include <Tests.h>
using namespace FM;

#include <QtDebug>
#include <QDir>
#include <QStringList>

#include <IChunk.h>
#include <Protos/common.pb.h>
#include <Common/PersistantData.h>
#include <Exceptions.h>
#include <priv/Constants.h>

Tests::Tests()
{
}

void Tests::initTestCase()
{
   Common::PersistantData::rmValue(FILE_CACHE); // Reset the stored cache.
   this->fileManager = Builder::newFileManager();
}

void Tests::addASharedDirectory()
{
   return;

   this->sharedDirsReadOnly << QDir::currentPath().append("/../../sharedDirs/share1");
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
   QStringList paths = this->fileManager->getSharedDirsReadOnly();
   QVERIFY(paths.size() == 1);
   QCOMPARE(paths.at(0), QDir::cleanPath(this->sharedDirsReadOnly.at(0)));
   QVERIFY(this->fileManager->getSharedDirsReadWrite().size() == 0);
}

void Tests::addAnAlreadySharedDirectory()
{
   return;

   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
   QStringList paths = this->fileManager->getSharedDirsReadOnly();
   QVERIFY(paths.size() == 1);
   QCOMPARE(paths.at(0), QDir::cleanPath(this->sharedDirsReadOnly.at(0)));
   QVERIFY(this->fileManager->getSharedDirsReadWrite().size() == 0);
}

void Tests::addInexistingSharedDirectory()
{
   return;

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
   return;

   this->sharedDirsReadOnly << QDir::currentPath().append("/../../sharedDirs/share1/subdir");
   this->sharedDirsReadOnly << QDir::currentPath().append("/../../sharedDirs/share1/anotherSubdir");
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
   return;

   QStringList sharedWriteDirs;
   sharedWriteDirs << QDir::currentPath().append("/../../sharedDirs");

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
   return;

   this->sharedDirsReadOnly << "C:/Qt/2009.05-rc1/bin";
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);

   return;

   /*qsrand(QTime(0,0,0).secsTo(QTime::currentTime()));
   QTest::qSleep(qrand() % 256 + 1);*/

   this->sharedDirsReadOnly << QDir::currentPath().append("/../../sharedDirs");
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
}

void Tests::rmSharedDirectory()
{
   return;
   this->sharedDirsReadOnly.clear();
   this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
}

void Tests::createAFile()
{
   return;
   QFile::remove("../../sharedDirs/incoming1/my_lol_cat.avi.unfinished");

   this->sharedDirsReadWrite << QDir::currentPath().append("/../../sharedDirs/incoming1");
   this->fileManager->setSharedDirsReadWrite(this->sharedDirsReadWrite);

   Protos::Common::FileEntry fileEntry;
   Protos::Common::Entry* entry = fileEntry.mutable_file();
   entry->set_path("");
   entry->set_name("my_lol_cat.avi");
   entry->set_size(650000000);
   QList< QSharedPointer<FM::IChunk> > chunks = this->fileManager->newFile(fileEntry);
}

void Tests::browseAdirectory()
{
   return;
   Protos::Core::GetEntriesResult entries = this->fileManager->getEntries();
   qDebug() << QString::fromStdString(entries.DebugString());

   Protos::Core::GetEntriesResult entries2 = this->fileManager->getEntries(entries.dir(0));
   qDebug() << QString::fromStdString(entries2.DebugString());
}

void Tests::cleanupTestCase()
{
   // This call is only used to stop the fileUpdater and wait for it to finish.
   // It's should not be used in a normal code.
   //this->fileManager.clear();
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

   this->sharedDirsReadOnly << QDir::currentPath().append("/../../aSharedDir");
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

