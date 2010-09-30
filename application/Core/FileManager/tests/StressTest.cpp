#include <StressTest.h>
using namespace FM;

#include <QTest>
#include <QtDebug>
#include <QtGlobal>
#include <QTime>
#include <QDirIterator>
#include <QMutex>
#include <QMutexLocker>

#include <Common/Global.h>

#include <Exceptions.h>

#include <Builder.h>

// Beautiful macros..
#define PROB_100(A) if (this->randGen.percentRand() <= 100 - A) return;
#define PROB_1000(A) if (this->randGen.permilRand() <= 1000 - A) return;

const QDir ROOT_DIR("stress");

int RandGenerator::percentRand()
{
   return this->mtrand.randInt(100);
}

int RandGenerator::permilRand()
{
   return this->mtrand.randInt(1000);
}

int RandGenerator::rand(int n)
{
   return this->mtrand.randInt(n);
}

QString RandGenerator::generateAName()
{
   QString str("");

   for (int j = 0; j < this->rand(3) + 1; j++)
   {
      if (!str.isEmpty())
         str.append(' ');

      for (int i = 0; i < this->rand(1) + 3; i++)
         str.append('A' + static_cast<char>(rand(25)));
   }
   return str;
}

FilesAndDirs::FilesAndDirs(QSharedPointer<IFileManager> fileManager, StressTest* stressTest)
   : fileManager(fileManager), stressTest(stressTest)
{
}

void FilesAndDirs::run()
{
   forever
   {
      (this->*FilesAndDirs::actions[this->randGen.rand(NB_ACTION-1)])();
      QTest::qWait(50);

      for (QMutableListIterator<QString> i(this->dirsToDelete); i.hasNext();)
      {
         QString dir = i.next();
         if (Common::Global::recursiveDeleteDirectory(dir))
         {
            qDebug() << "Directory removed : " << dir;
            i.remove();
         }
      }

      for (QMutableListIterator<QString> i(this->directories); i.hasNext();)
      {
         if (!QDir(i.next()).exists())
            i.remove();
      }
   }
}

void (FilesAndDirs::*FilesAndDirs::actions[])() =
{
   &FilesAndDirs::createADir,
   &FilesAndDirs::removeADir,
   &FilesAndDirs::createAFile,
   &FilesAndDirs::removeAFile,
};

const int FilesAndDirs::NB_ACTION = sizeof(FilesAndDirs::actions) / sizeof(void (FilesAndDirs::*)());

void FilesAndDirs::createADir()
{
   PROB_100(30);

   QStringList dirs;
   dirs.append(this->stressTest->getSharedDirsReadOnly());
   dirs.append(this->stressTest->getSharedDirsReadWrite());
   dirs.append(this->directories);

   if (dirs.empty())
      return;

   qDebug() << "===== StressTest::createADir() =====";

   QString dir = dirs[this->randGen.rand(dirs.size()-1)];
   QString name = this->randGen.generateAName();
   if (QDir(dir).mkdir(name))
   {
      QString path = dir.append("/").append(name);
      this->directories.append(path);
      qDebug() << "Create directory : " << path;
   }
   else
      qDebug() << "Can't create the directory " << name << " in " << dir;
}

void FilesAndDirs::removeADir()
{
   PROB_100(2);

   if (this->directories.empty())
      return;

   qDebug() << "===== StressTest::removeADir() =====";

   QString dir = this->directories.takeAt(this->randGen.rand(this->directories.size()-1));
   this->dirsToDelete << dir;
}

void FilesAndDirs::createAFile()
{
   PROB_100(60);

   QStringList dirs;
   dirs.append(this->stressTest->getSharedDirsReadOnly());
   dirs.append(this->stressTest->getSharedDirsReadWrite());
   dirs.append(this->directories);

   if (dirs.empty())
      return;

   qDebug() << "===== StressTest::createAFile() =====";

   QString filePath = dirs[this->randGen.rand(dirs.size()-1)] + "/" + this->randGen.generateAName();

   // Create a file with a random size from 1 to 100 Mo
   int bytes = this->randGen.rand(100 * 1024 * 1024 - 1) + 1;
   static const int CHUNK_SIZE = 64 * 1024;
   char buffer[CHUNK_SIZE];

   qDebug() << "Creating file " << filePath << " (" << Common::Global::formatByteSize(bytes) << ")";

   QFile file(filePath);
   if (!file.open(QIODevice::WriteOnly))
   {
      qDebug() << "Unable to open file " << filePath;
      return;
   }

   while (bytes > 0)
   {
      //qDebug() << bytes;
      int bufferSize = bytes < CHUNK_SIZE ? bytes : CHUNK_SIZE;

      int n = this->randGen.rand(255); // Don't generate a number for each byte to avoid too much computation.
      for (int i = 0; i < bufferSize; i++)
         buffer[i] = n++;

      int bytesWritten = file.write(buffer, bufferSize);
      if (bytesWritten == -1)
      {
         qDebug() << "Unable to write in the file " << filePath;
         break;
      }
      bytes -= bytesWritten;
   }
}

void FilesAndDirs::removeAFile()
{
   PROB_100(8);

   if (this->directories.isEmpty())
      return;

   qDebug() << "===== StressTest::removeAFile() =====";

   QString dirPath;
   QString filename;

   QStringList dirs(this->directories);
   do
   {
      if (dirs.isEmpty())
         return;

      int i = this->randGen.rand(dirs.size()-1);
      dirPath = dirs[i];
      QDir dir(dirPath);
      QStringList files = dir.entryList(QDir::Files);
      if (!files.isEmpty())
         filename = files[this->randGen.rand(files.size()-1)];
      else
         dirs.removeAt(i);

   } while (filename.isNull());

   if (!QDir(dirPath).remove(filename))
      qDebug() << "Unable to remove : " << dirPath.append("/").append(filename);
   else
      qDebug() << "File " << dirPath.append("/").append(filename) << " removed";
}


const int StressTest::NB_FILES_AND_DIR_THREAD = 3;

StressTest::StressTest()
{
   this->fileManager = Builder::newFileManager();

   QDir().mkdir(ROOT_DIR.dirName());
   qDebug() << Common::Global::recursiveDeleteDirectoryContent(ROOT_DIR.dirName());

   for (int i = 0; i < NB_FILES_AND_DIR_THREAD; i++)
   {
      FilesAndDirs* filesAndDirs = new FilesAndDirs(this->fileManager, this);
      this->filesAndDirs << filesAndDirs;
      filesAndDirs->start();
   }

   forever
   {
      (this->*StressTest::actions[this->randGen.rand(NB_ACTION-1)])();
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

QStringList StressTest::getSharedDirsReadOnly() const
{
   return this->sharedDirsReadOnly;
}

QStringList StressTest::getSharedDirsReadWrite() const
{
   return this->sharedDirsReadWrite;
}

void (StressTest::*StressTest::actions[])() =
{
   &StressTest::createASharedDir,
   &StressTest::removeASharedDir,
   &StressTest::doASearch,
   &StressTest::printAmount,
};

const int StressTest::NB_ACTION = sizeof(StressTest::actions) / sizeof(void (StressTest::*)());

void StressTest::createASharedDir()
{
   PROB_100(10);

   // No more than 30 shared directory
   if (this->sharedDirsReadOnly.size() + this->sharedDirsReadWrite.size() >= 30)
      return;

   qDebug() << "===== StressTest::createASharedDir() =====";

   QString name = this->randGen.generateAName();
   ROOT_DIR.mkdir(name);

   try
   {
      if (this->randGen.percentRand() <= 75)
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
      if (this->randGen.percentRand() <= 75)
      {
         if (this->sharedDirsReadOnly.isEmpty())
            return;
         int i = this->randGen.rand(this->sharedDirsReadOnly.size()-1);
         this->dirsToDelete << this->sharedDirsReadOnly[i];
         this->sharedDirsReadOnly.removeAt(i);
         this->fileManager->setSharedDirsReadOnly(this->sharedDirsReadOnly);
      }
      else
      {
         if (this->sharedDirsReadWrite.isEmpty())
            return;
         int i = this->randGen.rand(this->sharedDirsReadWrite.size()-1);
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

void StressTest::doASearch()
{
   PROB_100(10);

   qDebug() << "===== StressTest::doSomeSearchs() =====";

   const int N = 1000;
   bool found = false;
   QTime time;
   time.start();
   for (int n = 0; n < N; n ++)
   {
      QString terms = this->randGen.generateAName() + " " + this->randGen.generateAName();

      Protos::Common::FindResult result = this->fileManager->find(terms);

      if (result.entry_size() != 0)
      {
         found = true;
         qDebug() << "Found, terms : " << terms;
         for (int i = 0; i < result.entry_size(); i++)
            qDebug() << "[" << result.entry(i).level() << "] " << result.entry(i).entry().name().data();
      }
   }
   int delta = time.elapsed();
   qDebug() << "Time elapsed for " << N << " searchs : " << delta << " ms";

   if (!found)
      qDebug() << "Nothing found";
}

void StressTest::printAmount()
{
   PROB_100(20);

   qDebug() << "===== StressTest::printAmount() =====";

   qDebug() << "Amount of shared data : " << Common::Global::formatByteSize(this->fileManager->getAmount()) << " (" << this->fileManager->getAmount() << ")";
}

