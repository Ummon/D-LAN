#ifndef COMMON_TESTS_H
#define COMMON_TESTS_H

#include <QTest>

#include <PersistantData.h>
using namespace Common;

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
};

#endif
