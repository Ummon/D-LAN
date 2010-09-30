#include <StressTest.h>
using namespace FM;

#include <QTest>
#include <QtDebug>
#include <QtGlobal>
#include <QTime>
#include <QDirIterator>

#include <Common/Global.h>

#include <Exceptions.h>

#include <Builder.h>

const QDir StressTest::ROOT_DIR("stress");
MTRand StressTest::mtrand;

// Beautiful macros.
#define PROB_100(A) if (percentRand() <= 100 - A) return;
#define PROB_1000(A) if (permilRand() <= 1000 - A) return;

StressTest::StressTest()
{
   this->fileManager = Builder::newFileManager();

   QDir().mkdir(ROOT_DIR.dirName());
   qDebug() << Common::Global::recursiveDeleteDirectoryContent(ROOT_DIR.dirName());

   forever
   {
      (this->*StressTest::actions[mtrand.randInt(NB_ACTION-1)])();

      QTest::qWait(10);

      for (QMutableListIterator<QString> i(this->dirsToDelete); i.hasNext();)
      {
         QString dir = i.next();
         if (Common::Global::recursiveDeleteDirectory(dir))
         {
            qDebug() << "Directory removed : " << dir;
            i.remove();
         }
      }
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

   for (MTRand::uint32 j = 0; j < mtrand.randInt(2) + 1; j++)
   {
      if (!str.isEmpty())
         str.append(' ');

      for (MTRand::uint32 i = 0; i < mtrand.randInt(2) + 3; i++)
         str.append('A' + static_cast<char>(mtrand.randInt(25)));
   }
   return str;
}

int StressTest::percentRand()
{
   return mtrand.randInt(100);
}

int StressTest::permilRand()
{
   return mtrand.randInt(1000);
}

void StressTest::createASharedDir()
{
   PROB_100(10);

   // No more than 30 shared directory
   if (this->sharedDirsReadOnly.size() + this->sharedDirsReadWrite.size() >= 30)
      return;

   qDebug() << "===== StressTest::createASharedDir() =====";

   QString name = generateAName();
   ROOT_DIR.mkdir(name);

   try
   {
      if (this->percentRand() <= 75)
      {
         this->sharedDirsReadOnly << ROOT_DIR.dirName().append("/").append(name);
         this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
      }
      else
      {
         this->sharedDirsReadWrite << ROOT_DIR.dirName().append("/").append(name);;
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
   PROB_100(1);

   qDebug() << "===== StressTest::removeASharedDir() =====";
   try
   {
      if (this->percentRand() <= 75)
      {
         if (this->sharedDirsReadOnly.isEmpty())
            return;
         int i = mtrand.randInt(this->sharedDirsReadOnly.size()-1);
         this->dirsToDelete << this->sharedDirsReadOnly[i];
         this->sharedDirsReadOnly.removeAt(i);
         this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
      }
      else
      {
         if (this->sharedDirsReadWrite.isEmpty())
            return;
         int i = mtrand.randInt(this->sharedDirsReadWrite.size()-1);
         this->dirsToDelete << this->sharedDirsReadWrite[i];
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
   PROB_100(30);

   QStringList dirs;
   dirs.append(this->sharedDirsReadOnly);
   dirs.append(this->sharedDirsReadWrite);
   dirs.append(this->directories);

   if (dirs.empty())
      return;

   qDebug() << "===== StressTest::createADir() =====";

   QString dir = dirs[mtrand.randInt(dirs.size()-1)];
   QString name = generateAName();
   if (QDir(dir).mkdir(name))
   {
      QString path = dir.append("/").append(name);
      this->directories.append(path);
      qDebug() << "Create directory : " << path;
   }
   else
      qDebug() << "Can't create the directory " << name << " in " << dir;
}

void StressTest::removeADir()
{
   PROB_100(2);

   if (this->directories.empty())
      return;

   qDebug() << "===== StressTest::removeADir() =====";

   QString dir = this->directories.takeAt(mtrand.randInt(this->directories.size()-1));
   this->dirsToDelete << dir;
}

void StressTest::createAFile()
{
   PROB_100(50);

   QStringList dirs;
   dirs.append(this->sharedDirsReadOnly);
   dirs.append(this->sharedDirsReadWrite);
   dirs.append(this->directories);

   if (dirs.empty())
      return;

   qDebug() << "===== StressTest::createAFile() =====";

   QString filePath = dirs[mtrand.randInt(dirs.size()-1)] + "/" + generateAName();

   // Create a file with a random size from 1 to 100 Mo
   int bytes = mtrand.randInt(100 * 1024 * 1024 - 1) + 1;
   static const int CHUNK_SIZE = 64 * 1024;
   static char buffer[CHUNK_SIZE];

   qDebug() << "Creating file " << filePath << " (" << Common::Global::formatByteSize(bytes) << ")";

   QFile file(filePath);
   if (!file.open(QIODevice::WriteOnly))
   {
      qDebug() << "Unable to open file " << filePath;
      return;
   }

   while (bytes > 0)
   {
      int bufferSize = bytes < CHUNK_SIZE ? bytes : CHUNK_SIZE;
      for (int i = 0; i < bufferSize; i++)
         buffer[i] = mtrand.randInt(255);

      int bytesWritten = file.write(buffer, bufferSize);
      if (bytesWritten == -1)
      {
         qDebug() << "Unable to write in the file " << filePath;
         break;
      }
      bytes -= bytesWritten;
   }
}

void StressTest::removeAFile()
{
   PROB_100(0);

   qDebug() << "===== StressTest::removeAFile() =====";

}

