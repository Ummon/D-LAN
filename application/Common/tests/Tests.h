#ifndef TESTS_COMMON_H
#define TESTS_COMMON_H

#include <QTest>

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void writePersistantData();
   void readPersistantData();
   void removePersistantData();
   void generateAHash();
   void buildAnHashFromAString();
   void compareTwoHash();
   void math();
   void availableDiskSpace();
   void readAndWriteWithZeroCopyStreamQIODevice();
};

#endif
