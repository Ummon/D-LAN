#include <QtCore/QDebug>

#if defined(Q_OS_LINUX)
#include <priv/FileUpdater/WaitConditionLinux.h>
using namespace FM;

WaitConditionLinux::WaitConditionLinux()
{
}

WaitConditionLinux::~WaitConditionLinux()
{
}

void WaitConditionLinux::release()
{
   this->mutex.lock();
   this->waitCondition.wakeOne();
   this->mutex.unlock();      
}

void WaitConditionLinux::wait()
{
   this->mutex.lock();
   this->waitCondition.wait(&this->mutex);
   this->mutex.unlock();   
}

void* WaitConditionLinux::getHandle()
{
   return 0;
}

#endif
