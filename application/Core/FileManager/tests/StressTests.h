#ifndef TESTS_STRESSTESTS_H
#define TESTS_STRESSTESTS_H

#include <QObject>

class StressTests : public QObject
{
   Q_OBJECT
public:
    StressTests();

private slots:
    /***** Simulating of a real usage with all previous tests running concurrently *****/
    void stressTest();
};

#endif
