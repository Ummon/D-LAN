#include <QtCore/QtCore> // For the Q_OS_* defines.

#if defined(Q_OS_DARWIN)

#include <priv/FileUpdater/WaitConditionDarwin.h>
using namespace FM;

WaitConditionDarwin::WaitConditionDarwin() :
   released(false)
{
}

WaitConditionDarwin::~WaitConditionDarwin()
{
}

void WaitConditionDarwin::release()
{
   this->mutex.lock();
   this->released = true;
   this->waitCondition.wakeOne();
   this->mutex.unlock();
}

bool WaitConditionDarwin::wait(int timeout)
{
   bool timeouted = false;

   this->mutex.lock();
   if (!this->released)
      timeouted = !this->waitCondition.wait(&this->mutex, timeout == -1 ? ULONG_MAX : timeout);

   this->released = false;
   this->mutex.unlock();
   return timeouted;
}

void* WaitConditionDarwin::getHandle()
{
   return 0;
}

#endif
