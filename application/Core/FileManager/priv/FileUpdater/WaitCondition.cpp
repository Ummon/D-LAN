#include <priv/FileUpdater/WaitCondition.h>
using namespace FM;

#include <QtCore/QtCore> // For the Q_OS_* defines.

#include <priv/Log.h>

#if defined(Q_OS_WIN32)
   #include <priv/FileUpdater/WaitConditionWin.h>
#elif defined(Q_OS_LINUX)
   #include <priv/FileUpdater/WaitConditionLinux.h>
#endif

WaitCondition* WaitCondition::getNewWaitCondition()
{
#if defined(Q_OS_WIN32)
   return new WaitConditionWin();
#elif defined(Q_OS_LINUX)
   return new WaitConditionLinux();   
#else
   LOG_WARN("Cannot create a WaitCondition for the current platform, no implementation.");
   return 0;
#endif
}


