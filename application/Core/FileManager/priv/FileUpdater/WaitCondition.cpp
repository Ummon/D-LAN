#include <priv/FileUpdater/WaitCondition.h>
using namespace FM;

#include <QtCore/QtCore> // For the Q_OS_* defines.

#if defined(Q_OS_WIN32)
   #include <priv/FileUpdater/WaitConditionWin.h>
#endif

WaitCondition* WaitCondition::getNewWaitCondition()
{
#if defined(Q_OS_WIN32)
   return new WaitConditionWin();
#else
   LOG_WARN("Cannot create a WaitCondition for the current platform, no implementation.");
   return 0;
#endif
}


