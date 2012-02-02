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

#include <QtCore/QDebug>

#include <stdint.h>
#include <signal.h>
#include <fcntl.h>

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
      L_ERRO("WaitConditionLinux::WaitConditionLinux : Unable to create pipe.");

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
   L_DEBU(QString("WaitConditionLinux::release : begin write in pipe for read in fd=%1").arg(this->pfd[0]));
   write(this->pfd[1], "",1);
   L_DEBU(QString("WaitConditionLinux::release : end write in pipe for read in fd=%1").arg(this->pfd[0]));

   this->released = true;
}

bool WaitConditionLinux::wait(int timeout)
{
   if(this->released)
   {
      this->released = false;
      return true;
   }

   struct timeval time;
   fd_set fds;

   // Convert timeout in timeval
   time.tv_sec = timeout / 1000;
   time.tv_usec = (timeout % 1000) * 1000;

   // Zero-out the fd_set.
   FD_ZERO(&fds);

   // Add the inotify fd to the fd_set.
   FD_SET(this->pfd[0], &fds);

   L_DEBU(QString("WaitConditionLinux::wait : active select for fd=%1").arg(this->pfd[0]));
   if(select(this->pfd[0] + 1, &fds, NULL, NULL, (timeout==-1 ? 0 : &time)))
   {
      L_DEBU(QString("WaitConditionLinux::wait : exit select by release (fd=%1)").arg(this->pfd[0]));
      static char dummy[4096];
      while (read(this->pfd[0], dummy, sizeof(dummy)) > 0);
      return false;
   }

   return true;
}

int WaitConditionLinux::getFd()
{
   return this->pfd[0];
}

