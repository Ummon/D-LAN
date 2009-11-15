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
   void addASharedDirectory();
   void addAnAlreadySharedDirectory();
   void cleanupTestCase();
   void addInexistingSharedDirectory();
   void addSubSharedDirectories();
   void addSuperSharedDirectoriesWithDifferentRights();
   void addSuperSharedDirectoriesWithSameRights();
   void rmSharedDirectory();

private:
   void search();
   void addSuperSharedDirectoriesAndMerge();

private:
   void doASearch(bool checkResult);
   void printSearch(const QString& terms, const Protos::Common::FindResult& result);
   void printAmount();

   QStringList sharedDirs;
   QSharedPointer<IFileManager> fileManager;
};

#endif
