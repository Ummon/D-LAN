#ifndef FILE_MANAGER_TESTS_H
#define FILE_MANAGER_TESTS_H

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

   /***** Ask for chunks by hash *****/
   void getAExistingChunk();
   void getAUnexistingChunk();

   /***** Get Hashes from a FileEntry which the hash is already computed *****/
   void getHashesFromAFileEntry1();

   /***** Get Hashes from a FileEntry which the hash is unknown *****/
   void getHashesFromAFileEntry2();

   /***** Read and write concurrently some chunks *****/
   /*<Protos::Common::FileEntry fileEntry;
   Protos::Common::Entry* entry = fileEntry.mutable_file();
   entry->set_path("");
   entry->set_name("my_lol_cat.avi");
   entry->set_size(650000000);
   QList< QSharedPointer<FM::IChunk> > chunks = this->fileManager->newFile(fileEntry);*/

   /***** Browse the shared directories *****/
   void browseSomedirectories();

   /***** Find files and directories by keywords *****/
   void findExistingFilesWithOneWord();
   void findUnexistingFilesWithOneWord();
   void findFilesWithSomeWords();

   /***** Ask if the given hashes are known *****/
   void haveChunks();

   /***** Ask for the amount of shared byte *****/
   void printAmount();

   /***** Removing shared directories *****/
   void rmSharedDirectory();

   /***** Simulating of a real usage with all previous tests running concurrently *****/

   void cleanupTestCase();

private:
   void createInitialFiles();
   void deleteAllFiles();

   void search();
   void printSearch(const QString& terms, const Protos::Common::FindResult& result);
   void compareExpectedResult(const Protos::Common::FindResult& result, quint32 expectedLevelResult[], QList<QString> expectedFileResult[]);

   void addSuperSharedDirectoriesAndMerge();

   static void createFile(const QString& path);
   static void recursiveDeleteDirectory(const QString& dir);
   static void compareStrRegexp(const QString& regexp, const QString& str);

   QStringList sharedDirsReadOnly;
   QStringList sharedDirsReadWrite;
   QSharedPointer<IFileManager> fileManager;
};

#endif
