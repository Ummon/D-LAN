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

