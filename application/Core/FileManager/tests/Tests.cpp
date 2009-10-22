#include <Tests.h>
using namespace FM;

#include <QtDebug>
#include <QDir>
#include <QStringList>

#include <Protos/common.pb.h>

#include <Exceptions.h>

Tests::Tests()
{
}

void Tests::printAmount()
{
   qDebug() << "Sharing amount : " << this->fileManager->getAmount() << " bytes";
}

void Tests::doASearch(bool checkResult)
{
   QString terms("aaaa bbbb cccc");
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
   Protos::Common::FindResult result = this->fileManager->find(terms);
   qDebug() << "Search : '" << terms << "'";
   for (int i = 0; i < result.files_size(); i++)
   {
      qDebug() << "[" << result.files(i).level() << "] " << result.files(i).file().file().name().data();
      if (checkResult)
      {
         QVERIFY(result.files(i).level() == levelResults[i]);
         QVERIFY(fileResults[result.files(i).level()].contains(QString(result.files(i).file().file().name().data())));
      }
   }
}

void Tests::initTestCase()
{
   this->fileManager = Builder::newFileManager();
}

void Tests::addSharedDirectories()
{
   QStringList dirs;
   dirs << QDir::currentPath().append("/../../terms");
   dirs << QDir::currentPath().append("/asdasdasd"); // This directory doesn't exit.

   try
   {
      this->fileManager->setSharedDirsReadOnly(dirs);
   }
   catch (DirsNotFoundException& e)
   {
      foreach (QString path, e.getPaths())
         qDebug() << "This directory hasn't been found : " << path << " (Exception thrown)";
   }
}

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
}

void Tests::cleanupTestCase()
{
}

