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
   void initTestCase();

   // Global class.
   void nCombinations();
   void formatByteSize();
   void availableDiskSpace();

   // PersistentData class.
   void writePersistentData();
   void readPersistentData();
   void removePersistentData();

   // Settings class.
   void writeSettings();
   void readSettings();
   void removeSettings();

   // Hash class.
   void generateAHash();
   void buildAnHashFromAString();
   void compareTwoHash();
   void hasher();

   void messageHeader();

   // ZeroCopyOutputStreamQIODevice and ZeroCopyInputStreamQIODevice classes.
   void readAndWriteWithZeroCopyStreamQIODevice();

private:
   Common::Hash hash;
};

#endif
