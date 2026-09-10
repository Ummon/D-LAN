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
  
#include <priv/FileUpdater/WaitConditionLinux.h>
using namespace FM;

#include <unistd.h>

#include <QtCore/QDebug>

#include <stdint.h>
#include <signal.h>
#include <fcntl.h>
#include <poll.h>

/**
  * @class FM::WaitConditionLinux
  * @author Hervé Martinet
  *
  * Implementation of 'WaitCondition' for the Linux platform.
  */

WaitConditionLinux::WaitConditionLinux()
   : released(false)
{   
   if(0 != pipe(this->pfd))
      L_ERRO("WaitConditionLinux::WaitConditionLinux: Unable to create pipe.");

   fcntl(this->pfd[0],F_SETFL,fcntl(this->pfd[0],F_GETFL)|O_NONBLOCK);
   fcntl(this->pfd[1],F_SETFL,fcntl(this->pfd[1],F_GETFL)|O_NONBLOCK);
}

WaitConditionLinux::~WaitConditionLinux()
{
   close(this->pfd[0]);
   close(this->pfd[1]);
}

void WaitConditionLinux::release()
{
   L_DEBU(QString("WaitConditionLinux::release: begin write in pipe for read in fd=%1").arg(this->pfd[0]));
   write(this->pfd[1], "", 1);
   L_DEBU(QString("WaitConditionLinux::release: end write in pipe for read in fd=%1").arg(this->pfd[0]));

   this->released = true;
}

bool WaitConditionLinux::wait(int timeout)
{
   if(this->released)
   {
      this->released = false;
      return true;
   }

   // poll() supports descriptors above FD_SETSIZE and uses milliseconds,
   // including -1 for an indefinite wait, just like this method's API.
   pollfd fd{this->pfd[0], POLLIN, 0};
   L_DEBU(QString("WaitConditionLinux::wait: active poll for fd=%1").arg(this->pfd[0]));
   const int ready = poll(&fd, 1, timeout);
   if (ready == 0)
      return true;

   if (ready > 0 && (fd.revents & POLLIN))
   {
      L_DEBU(QString("WaitConditionLinux::wait: exit poll by release (fd=%1)").arg(this->pfd[0]));
      char dummy[4096];
      while (read(this->pfd[0], dummy, sizeof(dummy)) > 0);
   }
   else
      L_ERRO("WaitConditionLinux::wait: poll failed or woke without readable data.");

   return false;
}

int WaitConditionLinux::getFd()
{
   return this->pfd[0];
}
