
#include <QtCore/QtCore> // For the Q_OS_* defines.

#if !defined(FILEMANAGER_WAITCONDITIONDARWIN_H) && defined(Q_OS_DARWIN)
#define FILEMANAGER_WAITCONDITIONDARWIN_H

#include <QMutex>
#include <QWaitCondition>

#include <priv/FileUpdater/WaitCondition.h>

namespace FM
{
   class WaitConditionDarwin : public WaitCondition
   {
   public:
      WaitConditionDarwin();
      ~WaitConditionDarwin();

      void release();
      bool wait(int timeout = -1);
      void* getHandle();

   private:
      bool released;
      QMutex mutex;
      QWaitCondition waitCondition;
   };
}
#endif
