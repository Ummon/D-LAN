#ifndef COMMON_TESTS_H
#define COMMON_TESTS_H

#include <QTest>

#include <Builder.h>
#include <IFileManager.h>
using namespace FileManager;

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private:
   QSharedPointer<IFileManager> fileManager;

private slots:
   void initTestCase();
   void addSharedDirectories();
   void search();
   void cleanupTestCase();
};

#endif
