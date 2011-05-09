#ifndef FILEMANAGER_WAITCONDITIONDARWIN_H
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

   private:
      bool released;
      QMutex mutex;
      QWaitCondition waitCondition;
   };
}
#endif
