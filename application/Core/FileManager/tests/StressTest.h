#ifndef TESTS_FILEMANAGER_STRESSTEST_H
#define TESTS_FILEMANAGER_STRESSTEST_H

#include <QSharedPointer>
#include <QDir>
#include <QThread>

#include <Libs/MersenneTwister.h>

#include <IFileManager.h>
#include <IGetHashesResult.h>
using namespace FM;

class RandGenerator
{
public:
   int percentRand();
   int permilRand();
   int rand(int n);
   QString generateAName();

private:
   MTRand mtrand;
};

class StressTest;

class FilesAndDirs : public QThread
{
public:
   FilesAndDirs(QSharedPointer<IFileManager>, StressTest* stressTest);
   void run();

private:
   static void (FilesAndDirs::*actions[])();
   static const int NB_ACTION;

   void createADir();
   void removeADir();
   void createAFile();
   void removeAFile();

   QSharedPointer<IFileManager> fileManager;
   StressTest* stressTest;

   QStringList directories;
   QStringList dirsToDelete;

   RandGenerator randGen;
};

class StressTest : public QObject
{
   Q_OBJECT
   static const int NB_FILES_AND_DIR_THREAD;
public:
   StressTest();
   QStringList getSharedDirsReadOnly() const;
   QStringList getSharedDirsReadWrite() const;

private:
   static void (StressTest::*actions[])();
   static const int NB_ACTION;

   void createASharedDir();
   void removeASharedDir();
   void doASearch();
   void printAmount();
   void getChunk();
   void newFile();
   void haveChunk();
   void getRootEntries();
   void getEntries();
   void getHashes();

private slots:
   void nextHash(Common::Hash hash);

private:
   void addEntries(const Protos::Core::GetEntriesResult& entries);

   static QString entryToStr(const Protos::Common::Entry& entry);

   QSharedPointer<IFileManager> fileManager;
   QList<FilesAndDirs*> filesAndDirs;
   QStringList sharedDirsReadOnly;
   QStringList sharedDirsReadWrite;
   QStringList dirsToDelete;

   QList<Common::Hash> someHashes;
   QList<Protos::Common::Entry> knownDirEntries;
   QList<Protos::Common::Entry> knownFileEntries;

   QList< QSharedPointer<IGetHashesResult> > getHashesResults;
   QList<int> getHashesResultsNumber;

   RandGenerator randGen;
};

#endif
