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
  
#include <QtCore/QtCore> // For the Q_OS_* defines.

#if defined(Q_OS_LINUX)

#include <QtCore/QDebug>

#include <priv/FileUpdater/WaitConditionLinux.h>
using namespace FM;

//#include <sys/select.h>

WaitConditionLinux::WaitConditionLinux()
   : released(false)
{
   int fds[2];
   if (pipe(fds) == -1 ) {
       /* an error occurred */
   }
   this->fd = fds[1];
   close(fds[2]);
}

WaitConditionLinux::~WaitConditionLinux()
{
   close(this->fd);
}

void WaitConditionLinux::release()
{
   char b = '0';
   write(this->fd, &b, sizeof(b));
}

bool WaitConditionLinux::wait(int timeout)
{
   struct timeval time;
   fd_set fds;

   /* Convert timeout in timeval */
   time.tv_sec = timeout / 1000;
   time.tv_usec = (timeout % 1000) * 1000;

   /* Zero-out the fd_set. */
   FD_ZERO(&fds);

   /* Add the inotify fd to the fd_set. */
   FD_SET(this->fd, &fds);

   if(select(this->fd + 1, &fds, NULL, NULL, (timeout==-1 ? 0 : &time)))
      return false;

   return true;
}

int WaitConditionLinux::getHandle()
{
   return fd;
}

#endif

