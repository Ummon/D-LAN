#ifndef COMMON_IRUNNABLE_H
#define COMMON_IRUNNABLE_H

#include <QThread>

namespace Common
{
   /**
     * Methods 'init(..)' and 'finished()' are called in the main thread. Method 'run()' is called in a thread dedicated to the runnable object.
     */
   class IRunnable
   {
   public:
      virtual ~IRunnable() {}

      virtual void init(QThread* thread) = 0;
      virtual void run() = 0;
      virtual void finished() = 0;
      //virtual void stop() = 0;
   };
}

#endif
