#ifndef FILEMANAGER_WAITCONDITION_H
#define FILEMANAGER_WAITCONDITION_H

namespace FM
{
   /**
     * A wait condition used instead of the Qt implementation (QWaitCondidition) for
     * only one reason : to be able to use it natively with DirWatcher to wait.
     */
   class WaitCondition
   {
   public:
      /**
        * Build a new wait condition which depends from the current platform.
        */
      static WaitCondition* getNewWaitCondition();

      virtual ~WaitCondition() {}

      /**
        * Set the 'WaitCondition' in released state. 
        *  - The next call to wait will not block
        *  - If there is already a wait thread then it will be imediately released.
        * Non-blocking call.
        */
      virtual void release() = 0;

      /**
        * Wait for a release.
        * If a release has previously be asked then the first call will not block.
        * /!\ It's not the same behaviour than QWaitCondition!
        * @param timeout After the given time the condition will be auto released. -1 means wait forever (no timeout).
        * @return true if timeouted
        */
      virtual bool wait(int timeout = -1) = 0;
         
      /**
        * Return a pointer to a native structure.
        * For example, on Windows it will return a HANDLE on the event.
        * It can then be used with primitive like 'WaitForMultipleObjects'.
        */
      virtual void* getHandle() = 0;
   };
}
#endif
