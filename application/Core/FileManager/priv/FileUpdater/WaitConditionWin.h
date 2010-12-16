#include <QtCore/QtCore> // For the Q_OS_* defines.

#if !defined(FILEMANAGER_WAITCONDITIONWIN_H) and defined(Q_OS_WIN32)
#define FILEMANAGER_WAITCONDITIONWIN_H

#include <priv/FileUpdater/WaitCondition.h>

#include <windows.h>

namespace FM
{
   /**
     * Implementation of 'WaitCondition' for the Windows platform.
     * see : http://msdn.microsoft.com/en-us/library/ms686211%28VS.85%29.aspx add associated pages.
     */
   class WaitConditionWin : public WaitCondition
   {
   public:
      WaitConditionWin();
      ~WaitConditionWin();

      void release();
      bool wait(int timeout = -1);
      void* getHandle();

   private:
      HANDLE handle;
   };
}

#endif
