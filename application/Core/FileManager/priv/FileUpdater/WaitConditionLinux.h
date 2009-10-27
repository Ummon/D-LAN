#include <QtCore/QtCore> // For the Q_OS_* defines.

#if !defined(FILEMANAGER_WAITCONDITIONLINUX_H) and defined(Q_OS_LINUX)
#define FILEMANAGER_WAITCONDITIONLINUX_H

#include <QMutex>
#include <QWaitCondition>

#include <priv/FileUpdater/WaitCondition.h>

namespace FM
{
   /**
     * Implementation of 'WaitCondition' for the Linux platform.
     */
   class WaitConditionLinux : public WaitCondition
   {
   public:
      WaitConditionLinux();
      ~WaitConditionLinux();

      void release();
      void wait();
      void* getHandle();

   private:
      QMutex mutex;
      QWaitCondition waitCondition;
   };
}

#endif
