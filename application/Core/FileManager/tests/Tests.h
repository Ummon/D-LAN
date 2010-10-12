#ifndef TESTS_FILEMANAGER_H
#define TESTS_FILEMANAGER_H

#include <QTest>
#include <QDir>

#include <Protos/common.pb.h>

#include <Builder.h>
#include <IFileManager.h>
using namespace FM;

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();
   void createFileManager();

   /***** Adding shared directories *****/
   void addASharedDirectory();
   void addAnAlreadySharedDirectory();
   void addInexistingSharedDirectory();
   void addSubSharedDirectories();
   void addSuperSharedDirectoriesWithDifferentRights();
   void addSuperSharedDirectoriesWithSameRights();
   void addASharedDirectoryReadWrite();

   /***** Modification of the file system *****/
   void createAFile();
   void moveAFile();
   void renameAFile();
   void modifyAFile();
   void removeAFile();
   void createASubFile();
   void createABigFile();
   void modifyABigFile();
   void removeABigFile();
   void createADirectory();
   void renameADirectory();
   void moveAnEmptyDirectory();
   void moveADirectoryContainingFiles();
   void removeADirectory();
   void createAnEmptyFile();

   /***** Ask for chunks by hash *****/
   void getAExistingChunk();
   void getAUnexistingChunk();

   /***** Get Hashes from a FileEntry which the hash is already computed *****/
   void getHashesFromAFileEntry1();

   /***** Get Hashes from a FileEntry which the hash is unknown *****/
   void getHashesFromAFileEntry2();

   /***** Browse the shared directories *****/
   void browseSomedirectories();

   /***** Find files and directories by keywords *****/
   void findExistingFilesWithOneWord();
   void findUnexistingFilesWithOneWord();
   void findFilesWithSomeWords();
   void findFilesWithResultFragmentation();

   /***** Ask if the given hashes are known *****/
   void haveChunks();

   /***** Ask for the amount of shared byte *****/
   void printAmount();

   /***** Simulating of a real usage with all previous tests running concurrently *****/
   void stressTest();

   /***** Removing shared directories *****/
   void rmSharedDirectory();

   void cleanupTestCase();

private:
   void createInitialFiles();
   void deleteAllFiles();

   void printSearch(const QString& terms, const Protos::Common::FindResult& result);
   void compareExpectedResult(const Protos::Common::FindResult& result, quint32 expectedLevelResult[], QList<QString> expectedFileResult[]);

   void addSuperSharedDirectoriesAndMerge();

   static void compareStrRegexp(const QString& regexp, const QString& str);

   QStringList sharedDirsReadOnly;
   QStringList sharedDirsReadWrite;
   QSharedPointer<IFileManager> fileManager;
};

#endif
