/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
   if (!this->timer.isActive())
      connect(&this->timer, SIGNAL(timeout()), this, SLOT(timeoutSlot()), Qt::DirectConnection);

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
