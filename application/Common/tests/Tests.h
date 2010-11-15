#ifndef TESTS_COMMON_H
#define TESTS_COMMON_H

#include <QTest>

#include <Hash.h>

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void writePersistentData();
   void readPersistentData();
   void removePersistentData();
   void writeSettings();
   void readSettings();
   void removeSettings();
   void generateAHash();
   void buildAnHashFromAString();
   void compareTwoHash();
   void nCombinations();
   void availableDiskSpace();
   void readAndWriteWithZeroCopyStreamQIODevice();

private:
   Common::Hash hash;
};

#endif
