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

private:
   void printAmount();
   QSharedPointer<IFileManager> fileManager;

   void doASearch(bool checkResult);

private slots:
   void initTestCase();
   void addSharedDirectories();
   void search();
   void cleanupTestCase();
};

#endif
