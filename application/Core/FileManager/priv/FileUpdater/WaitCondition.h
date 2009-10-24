#ifndef FILEMANAGER_WAITCONDITION_H
#define FILEMANAGER_WAITCONDITION_H

namespace FM
{
   /**
     * A wait condition used instead of the Qt implementation (QWaitCondidition) for
     * only one reason : to be able to use it natively with DirWatcher to wait
     */
   class WaitCondition
   {
   public:
      static WaitCondition* getNewWaitCondition();

      virtual ~WaitCondition() {}

      virtual void release() = 0;

      /**
        * Wait for a release. If a release has previously be asked then he will
        * not be blocked.
        */
      virtual void wait() = 0;
      virtual void* getHandle() = 0;
   };
}
#endif
