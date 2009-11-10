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
   void addInexistantSharedDirectory();
   void addSubSharedDirectories();

private:
   void search();
   void addSuperSharedDirectories();
   void rmSharedDirectories();
   void addSuperSharedDirectoriesAndMerge();

private:
   void doASearch(bool checkResult);
   void printSearch(const QString& terms, const Protos::Common::FindResult& result);
   void printAmount();

   QStringList sharedDirs;
   QSharedPointer<IFileManager> fileManager;
};

#endif
