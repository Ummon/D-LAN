#ifndef TESTS_FILEMANAGER_STRESSTEST_H
#define TESTS_FILEMANAGER_STRESSTEST_H

#include <QSharedPointer>
#include <QDir>

#include <IFileManager.h>
using namespace FM;

class StressTest
{
   static const QDir ROOT_DIR;

public:
   StressTest();

private:
   static void (StressTest::*actions[])();
   static const int NB_ACTION;
   static QString generateAName();

   void createASharedDir();
   void removeASharedDir();
   void createADir();
   void removeADir();
   void createAFile();
   void removeAFile();

   void doAnAction();

   QSharedPointer<IFileManager> fileManager;

   QStringList sharedDirsReadOnly;
   QStringList sharedDirsReadWrite;
   QStringList directories;
   QStringList files;
};

#endif
