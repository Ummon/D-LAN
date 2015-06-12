#ifndef BENCHMARK_TESTS_H
#define BENCHMARK_TESTS_H

#include <QTest>

class BenchmarkTests : public QObject
{
   Q_OBJECT
public:
   BenchmarkTests();

private slots:
   void sortedArray();

};

#endif
