#ifndef CORE_COREAPPLICATION_H
#define CORE_COREAPPLICATION_H

#include <QCoreApplication>

class CoreApplication : public QCoreApplication
{
   Q_OBJECT

public:
   CoreApplication(int& argc, char** argv);
   bool winEventFilter(MSG* msg, long* result);

/*signals:
   void resumeFromLowPowerState();*/
};

#endif
