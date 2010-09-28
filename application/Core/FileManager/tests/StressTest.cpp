#include <StressTest.h>
using namespace FM;

#include <QTest>
#include <QtDebug>
#include <QtGlobal>
#include <QTime>

#include <Common/Global.h>

#include <Exceptions.h>

#include <Builder.h>

const QDir StressTest::ROOT_DIR("stress");

StressTest::StressTest()
{
   this->fileManager = Builder::newFileManager();
   Common::Global::recursiveDeleteDirectory(ROOT_DIR.dirName());
   QDir().mkdir(ROOT_DIR.dirName());

   forever
   {
      this->doAnAction();
      QTest::qWait(20);
   }
}

void (StressTest::*StressTest::actions[])() =
{
   &StressTest::createASharedDir,
   &StressTest::removeASharedDir,
   &StressTest::createADir,
   &StressTest::removeADir,
   &StressTest::createAFile,
   &StressTest::removeAFile,
};

const int StressTest::NB_ACTION = sizeof(StressTest::actions) / sizeof(void (StressTest::*)());

QString StressTest::generateAName()
{
   QString str("");
   for (int i = 0; i < 2; i++)
      str.append('A' + qrand() % 26);
   return str;
}

void StressTest::createASharedDir()
{
   if (qrand() % 101 >= 90)
      return;

   qDebug() << "===== StressTest::createASharedDir() =====";
   QString name = generateAName();
   ROOT_DIR.mkdir(name);

   try
   {
      if (qrand() % 101 <= 75)
      {
         this->sharedDirsReadOnly << ROOT_DIR.absolutePath().append("/").append(name);
         this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
      }
      else
      {
         this->sharedDirsReadWrite << ROOT_DIR.absolutePath().append("/").append(name);
         this->fileManager->setSharedDirsReadWrite(this->sharedDirsReadWrite);
      }
   }
   catch (DirsNotFoundException& e)
   {
      qDebug() << "createASharedDir() : This dirs wasn't found : " << e.paths;
   }
}

void StressTest::removeASharedDir()
{
   if (qrand() % 101 >= 95)
      return;

   qDebug() << "===== StressTest::removeASharedDir() =====";
   try
   {
      if (qrand() % 101 <= 75)
      {
         if (this->sharedDirsReadOnly.isEmpty())
            return;
         int i = qrand() % this->sharedDirsReadOnly.size();
         qDebug() << "Remove : " << QDir(this->sharedDirsReadOnly[i]).dirName();
         ROOT_DIR.rmpath(QDir(this->sharedDirsReadOnly[i]).dirName());
         this->sharedDirsReadOnly.removeAt(i);
         this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
      }
      else
      {
         if (this->sharedDirsReadWrite.isEmpty())
            return;
         int i = qrand() % this->sharedDirsReadWrite.size();
         qDebug() << "Remove : " << QDir(this->sharedDirsReadWrite[i]).dirName();
         ROOT_DIR.rmpath(QDir(this->sharedDirsReadWrite[i]).dirName());
         this->sharedDirsReadWrite.removeAt(i);
         this->fileManager->setSharedDirsReadWrite(this->sharedDirsReadWrite);
      }
   }
   catch (DirsNotFoundException& e)
   {
      qDebug() << "removeASharedDir() : This dirs wasn't found : " << e.paths;
   }
}

void StressTest::createADir()
{
   if (qrand() % 101 >= 50)
      return;

   qDebug() << "===== StressTest::createADir() =====";

   QStringList dirs;
   dirs.append(this->sharedDirsReadOnly);
   dirs.append(this->sharedDirsReadWrite);
   dirs.append(this->directories);

   if (dirs.empty())
      return;

   QString dir = dirs[qrand() % dirs.size()];
   QString name = generateAName();
   QDir(dir).mkdir(name);
   QString path = dir.append("/").append(name);
   this->directories.append(path);
   qDebug() << "Create directory : " << path;
}

void StressTest::removeADir()
{
   if (qrand() % 101 >= 70)
      return;

   qDebug() << "===== StressTest::removeADir() =====";

   if (this->directories.empty())
      return;

   QString dir = this->directories.takeAt(qrand() % this->directories.size());
   ROOT_DIR.rmpath(dir);
   qDebug() << "Remove directory : " << dir;
}

void StressTest::createAFile()
{
   qDebug() << "===== StressTest::createAFile() =====";

}

void StressTest::removeAFile()
{
   if (qrand() % 101 >= 25)
      return;

   qDebug() << "===== StressTest::removeAFile() =====";

}

void StressTest::doAnAction()
{
   qsrand(QTime(0,0,0).msecsTo(QTime::currentTime()));

   //void (StressTest::*actions[])() = { &StressTest::doAnAction };

   (this->*StressTest::actions[qrand() % NB_ACTION])();

//   switch (qrand() % NB_ACTION)
//   {
//   case 0 : this->createASharedDir(); break;
//   case 1 : this->removeASharedDir(); break;
//   case 0 : this->createADir(); break;
//   case 1 : this->removeADir(); break;
//   case 0 : this->createAFile(); break;
//   case 1 : this->removeAFile(); break;
//   }
}
