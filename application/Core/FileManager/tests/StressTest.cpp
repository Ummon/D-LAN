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
#include <Common/Settings.h>

#include <priv/Constants.h>
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
      QTest::qWait(this->randGen.rand(800) + 100);

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

   // Create a file with a random size from 1 B to 100 MB
   int bytes = this->randGen.rand(100 * 1024 * 1024 - 1) + 1;
   const int CHUNK_SIZE = SETTINGS.getUInt32("chunk_size");
   QByteArray buffer(CHUNK_SIZE, 0); // Don't know why "char buffer[CHUNK_SIZE]" crashes..

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

      int n = this->randGen.rand(255); // Don't generate a number for each byte to avoid too much computation.
      for (int i = 0; i < bufferSize; i++)
         buffer[i] = n++;

      int bytesWritten = file.write(buffer, bufferSize);
      if (bytesWritten == -1 || bytesWritten == 0)
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
   Common::Global::recursiveDeleteDirectoryContent(ROOT_DIR.dirName());

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
   &StressTest::getChunk,
   &StressTest::newFile,
   &StressTest::haveChunk,
   &StressTest::getRootEntries,
   &StressTest::getEntries,
   &StressTest::getHashes,
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

void StressTest::getChunk()
{
   PROB_100(20);

   if (this->someHashes.isEmpty())
      return;

   qDebug() << "===== StressTest::getChunk() =====";

   const int N = 10000;
   QTime time;
   time.start();

   int nbFound = 0;
   int nbNotFound = 0;
   int i;
   for (i = 0; i < N && !this->someHashes.empty(); i++)
   {
      int n = this->randGen.rand(this->someHashes.size()-1);
      Common::Hash hash = this->someHashes[n];
      try
      {
         QSharedPointer<IChunk> chunk = this->fileManager->getChunk(hash);
         nbFound++;

         if (this->randGen.permilRand() <= 1)
         {
            Uploader* uploader = new Uploader(chunk);
            uploader->start();
         }
      }
      catch(UnknownChunkException&)
      {
         nbNotFound++;
         this->someHashes.removeAt(n);
         //qDebug() << "Hash unknown : " << hash.toStr();
      }
   }
   qDebug() << "Time elapsed for " << i << " getChunk : " << time.elapsed() << " ms";
   qDebug() << "Number of chunk found : " << nbFound << ", not found : " << nbNotFound;
}

void StressTest::newFile()
{
   PROB_100(10);

   qDebug() << "===== StressTest::newFile() =====";

   QString path("/");
   for (int i = 0; i < this->randGen.rand(2); i++)
   {
      path.append(this->randGen.generateAName()).append("/");
   }

   // Size is from 1 B to 100 MB.
   int bytes = this->randGen.rand(100 * 1024 * 1024 - 1) + 1;
   static const int CHUNK_SIZE = SETTINGS.getUInt32("chunk_size");
   int nbChunk = bytes / CHUNK_SIZE + (bytes % CHUNK_SIZE == 0 ? 0 : 1);

   Protos::Common::Entry entry;
   entry.set_type(Protos::Common::Entry_Type_DIR);
   entry.set_path(path.toStdString());
   entry.set_name(this->randGen.generateAName().toStdString());
   entry.set_size(bytes);
   for (int i = 0; i < this->randGen.rand(nbChunk); i++) // Do not give all hashes.
   {
      Protos::Common::Hash* hash = entry.add_chunk();
      hash->set_hash(Common::Hash::rand().getData(), Common::Hash::HASH_SIZE);
   }

   try
   {
      QList< QSharedPointer<IChunk> > chunks = this->fileManager->newFile(entry);
      for (QListIterator< QSharedPointer<IChunk> > i(chunks); i.hasNext();)
      {
         Downloader* downloader = new Downloader(i.next(), QString(entry.path().data()) + QString(entry.name().data()));
         downloader->start();
      }
   }
   catch(NoReadWriteSharedDirectoryException&)
   {
      qDebug() << "NoReadWriteSharedDirectoryException";
   }
   catch(InsufficientStorageSpaceException&)
   {
      qDebug() << "InsufficientStorageSpaceException";
   }
   catch(FilePhysicallyAlreadyExistsException)
   {
      qDebug() << "FilePhysicallyAlreadyExistsException";
   }
   catch(UnableToCreateNewFileException&)
   {
      qDebug() << "UnableToCreateNewFileException";
   }
}

void StressTest::haveChunk()
{
   PROB_100(80);

   if (this->someHashes.isEmpty())
      return;

   qDebug() << "===== StressTest::haveChunk() =====";

   QList<Common::Hash> hashes;

   int n = this->randGen.rand(2000) + 500;
   for (int i = 0; i < n && i < this->someHashes.size(); i++)
      hashes << this->someHashes[i];

   QTime time;
   time.start();
   QBitArray result = this->fileManager->haveChunks(hashes);

   qDebug() << "Ask for " << hashes.size() << " hashe(s). Request time : " << time.elapsed() << " ms";
   QString resultStr(result.size(), '0');
   for (int i = 0; i < result.size(); i++)
      resultStr[i] = result[i] ? '1' : '0';
   qDebug() << resultStr;
}

void StressTest::getRootEntries()
{
   PROB_100(10);

   qDebug() << "===== StressTest::getRootEntries() =====";

   Protos::Core::GetEntriesResult roots = this->fileManager->getEntries();
   this->addEntries(roots);
}

void StressTest::getEntries()
{
   PROB_100(40);

   if (this->knownDirEntries.isEmpty())
      return;

   qDebug() << "===== StressTest::getEntries() =====";

   const int N = 30;
   int n;
   QTime time;
   time.start();
   for (n = 0; n < N && n < this->knownDirEntries.size(); n++)
   {
      int n = this->randGen.rand(this->knownDirEntries.size()-1);
      Protos::Common::Entry entry = this->knownDirEntries[n];
      if (!entry.has_shared_dir())
      {
         qDebug() << "!!! The entry " << entry.path().data() << entry.name().data() << " doesn't have a shared directory defined !!!";
         this->knownDirEntries.removeAt(n);
         return;
      }

      qDebug() << "Search : " << entryToStr(entry);

      Protos::Core::GetEntriesResult entries = this->fileManager->getEntries(entry);

      if (entries.entry_size() == 0)
      {
         qDebug() << "No sub entries found";
         this->knownDirEntries.removeAt(n);
         continue;
      }

      for (int i = 0; i < entries.entry_size(); i++)
         entries.mutable_entry(i)->mutable_shared_dir()->CopyFrom(entry.shared_dir());

      this->addEntries(entries);
   }
   qDebug() << "Time for " << n << " searches : " << time.elapsed() << " ms";
}

void StressTest::getHashes()
{
   PROB_100(40);

   if (this->knownFileEntries.isEmpty())
      return;

   qDebug() << "===== StressTest::getHashes() =====";

   Protos::Common::Entry entry = this->knownFileEntries[this->randGen.rand(this->knownFileEntries.size()-1)];

   qDebug() << "Entry " << entryToStr(entry);

   QSharedPointer<IGetHashesResult> result = this->fileManager->getHashes(entry);
   connect(result.data(), SIGNAL(nextHash(Common::Hash)), this, SLOT(nextHash(Common::Hash)), Qt::QueuedConnection);
   Protos::Core::GetHashesResult result2 = result->start();

   if (result2.status() != Protos::Core::GetHashesResult_Status_OK)
   {
      qDebug() << "Not found!";
   }
   else
   {
      qDebug() << "Number of hashes : " << result2.nb_hash();
      this->getHashesResults << HashesResult(result, result2.nb_hash(), entryToStr(entry));
   }
}

void StressTest::nextHash(Common::Hash hash)
{
   for (int i = 0; i < this->getHashesResults.size(); i++)
      if (this->getHashesResults[i].result.data() == dynamic_cast<IGetHashesResult*>(this->sender()))
      {
         qDebug() << "nextHash : " << hash.toStr() << " for " << this->getHashesResults[i].filename;
         if (!--this->getHashesResults[i].nb)
         {
            qDebug() << "It was the last hash! Removing the listener..";
            this->getHashesResults.removeAt(i);
         }
         break;
      }
}

void StressTest::addEntries(const Protos::Core::GetEntriesResult& entries)
{
   for (int i = 0; i < entries.entry_size(); i++)
   {
      // Check if you don't already have the entry
      for (int j = 0; j < this->knownDirEntries.size(); j++)
         if (this->knownDirEntries[j].path() == entries.entry(i).path() &&
             this->knownDirEntries[j].name() == entries.entry(i).name() &&
             this->knownDirEntries[j].shared_dir().id().hash() == entries.entry(i).shared_dir().id().hash())
            goto nextEntryResult;

      qDebug() << entryToStr(entries.entry(i));

      if (entries.entry(i).type() == Protos::Common::Entry_Type_DIR)
         this->knownDirEntries << entries.entry(i);
      else
      {
         this->knownFileEntries << entries.entry(i);
         for (int j = 0; j < entries.entry(i).chunk_size(); j++)
            this->someHashes << Common::Hash(entries.entry(i).chunk(j).hash().data());
      }

      nextEntryResult:;
   }
}

QString StressTest::entryToStr(const Protos::Common::Entry& entry)
{
   QString str = QString("Entry (%1), path = %2, name = %3, sharedDir = %4").
      arg(entry.type() == Protos::Common::Entry_Type_DIR ? "DIR" : "FILE").
      arg(entry.path().data()).
      arg(entry.name().data()).
      arg(Common::Hash(entry.shared_dir().id().hash().data()).toStr());

   if (entry.type() == Protos::Common::Entry_Type_FILE)
   {
      str.append(QString(" number of chunk : %1").arg(entry.chunk_size()));
      for (int i = 0; i < entry.chunk_size(); i++)
         str.append(" ").append(Common::Hash(entry.chunk(i).hash().data()).toStr());
   }
   return str;
}

Downloader::Downloader(QSharedPointer<IChunk> chunk, QString filePath)
   : chunk(chunk), filePath(filePath)
{
}

void Downloader::run()
{
   qDebug() << "===== Downloader::run() =====";

   try
   {
      QSharedPointer<IDataWriter> writer = chunk->getDataWriter();

      const int BUFFER_SIZE = SETTINGS.getUInt32("buffer_size");
      char buffer[BUFFER_SIZE];
      for (int i = 0; i < BUFFER_SIZE; i++)
         buffer[i] = this->chunk->getNum() + 1;

      while (!writer->write(buffer, BUFFER_SIZE));
   }
   catch(UnableToOpenFileInWriteModeException&)
   {
      qDebug() << "UnableToOpenFileInWriteModeException, file = " << this->filePath;
   }
   catch(IOErrorException&)
   {
      qDebug() << "IOErrorException, file = " << this->filePath;
   }
   catch(TryToWriteBeyondTheEndOfChunkException&)
   {
      qDebug() << "TryToWriteBeyondTheEndOfChunkException, file = " << this->filePath;
   }
   catch(ChunkDeletedException&)
   {
      qDebug() << "ChunkDeletedException, file = " << this->filePath;
   }

   qDebug() << "Downloader::run() finished, file" << this->filePath;
}

Uploader::Uploader(QSharedPointer<IChunk> chunk)
   : chunk(chunk)
{
}

void Uploader::run()
{
   qDebug() << "===== Uploader::run() =====";

   try
   {
      QSharedPointer<FM::IDataReader> reader = this->chunk->getDataReader();

      char buffer[SETTINGS.getUInt32("buffer_size")];

      qint64 bytesReadTotal = 0;
      qint64 bytesRead = 0;

      while (bytesRead = reader->read(buffer, bytesReadTotal))
         bytesReadTotal += bytesRead;
      qDebug() << "Uploader::run() : bytesRead = " << bytesReadTotal << " for the chunk " << this->chunk->getHash().toStr() << " num : " << this->chunk->getNum();
   }
   catch(UnableToOpenFileInReadModeException&)
   {
      qDebug() << "UnableToOpenFileInReadModeException";
   }
   catch(IOErrorException&)
   {
      qDebug() << "IOErrorException";
   }
   catch(ChunkDeletedException&)
   {
      qDebug() << "ChunkDeletedException";
   }
   catch(ChunkNotCompletedException&)
   {
      qDebug() << "ChunkNotCompletedException";
   }

   qDebug() << "Uploader::run() finished";
}

