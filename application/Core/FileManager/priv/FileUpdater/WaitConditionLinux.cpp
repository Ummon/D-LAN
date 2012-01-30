/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <QtCore/QDebug>

#if defined(Q_OS_LINUX)
#include <priv/FileUpdater/WaitConditionLinux.h>
using namespace FM;

WaitConditionLinux::WaitConditionLinux() :
   released(false)
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

bool WaitConditionLinux::wait(int timeout)
{
   bool timeouted = false;

   this->mutex.lock();
   if (!this->released)
      timeouted = !this->waitCondition.wait(&this->mutex, timeout == -1 ? ULONG_MAX : timeout);

   this->released = false;
   this->mutex.unlock();
   return timeouted;
}

void* WaitConditionLinux::getHandle()
{
   return 0;
}

#endif
