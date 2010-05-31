#ifndef COMMON_TESTS_H
#define COMMON_TESTS_H

#include <QTest>

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

   /***** Create / remove / modify the file system *****/
   void createAFile();

   /***** Ask for chunks by hash *****/

   /***** Get Hashes from a FileEntry which the hash is already computed *****/

   /***** Get Hashes from a FileEntry which the hash is unknown *****/

   /***** Read and write concurrently some chunks *****/

   /***** Browse the shared directories *****/
   void browseAdirectory();

   /***** Find files and directories by keywords *****/

   /***** Ask if the given hashes are known *****/

   /***** Ask for the amount of shared byte *****/

   /***** Removing shared directories *****/
   void rmSharedDirectory();

   /***** Simulating of a real usage with all previous tests running concurrently *****/

   void cleanupTestCase();

private:
   void search();
   void addSuperSharedDirectoriesAndMerge();
   void doASearch(bool checkResult);
   void printSearch(const QString& terms, const Protos::Common::FindResult& result);
   void printAmount();

   QStringList sharedDirsReadOnly;
   QStringList sharedDirsReadWrite;
   QSharedPointer<IFileManager> fileManager;
};

#endif
