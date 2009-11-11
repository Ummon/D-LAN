#include <QtCore/QDebug>

#if defined(Q_OS_LINUX)
#include <priv/FileUpdater/WaitConditionLinux.h>
using namespace FM;

WaitConditionLinux::WaitConditionLinux()
   : released(false)
{
}

WaitConditionLinux::~WaitConditionLinux()
{
}

void WaitConditionLinux::release()
{
   this->mutex.lock();
   this->released = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();      
}

void WaitConditionLinux::wait()
{
   this->mutex.lock();
   if (!this->released)
      this->waitCondition.wait(&this->mutex);
   
   this->released = false;
   this->mutex.unlock();   
}

void* WaitConditionLinux::getHandle()
{
   return 0;
}

#endif
