#include <Common/Timeoutable.h>
using namespace Common;

Timeoutable::Timeoutable(int time)
   : timeouted(false)
{
   this->timer.setInterval(time);
   this->timer.setSingleShot(true);
}

bool Timeoutable::isTimeouted() const
{
   return this->timeouted;
}

void Timeoutable::startTimer()
{
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(timeoutSlot()));
   this->timer.start();
}

void Timeoutable::stopTimer()
{
   disconnect(&this->timer, SIGNAL(timeout()), this, SLOT(timeoutSlot()));
   this->timer.stop();
}

void Timeoutable::timeoutSlot()
{
   this->timeouted = true;
   emit timeout();
}
