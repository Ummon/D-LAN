#pragma once

#include <QTest>

class BenchmarkTests : public QObject
{
   Q_OBJECT
public:
   BenchmarkTests();

private slots:
   void sortedArray();

};
