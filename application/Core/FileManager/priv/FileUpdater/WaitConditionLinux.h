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
     * For the moment it uses Qt classes but in the future it should use native
     * primitives to permit DirWatcherLinux to wait on a filesystem event AND on a WaitConditionLinux.
     * See the Windows implementation for more information.
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
      bool released;
      QMutex mutex;
      QWaitCondition waitCondition;
   };
}

#endif
