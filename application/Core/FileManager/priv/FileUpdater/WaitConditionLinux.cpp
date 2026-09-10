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
#include <QDeadlineTimer>

#include <stdint.h>
#include <signal.h>
#include <fcntl.h>
#include <poll.h>
#include <cerrno>
#include <system_error>

/**
  * @class FM::WaitConditionLinux
  * @author Hervé Martinet
  *
  * Implementation of 'WaitCondition' for the Linux platform.
  */

WaitConditionLinux::WaitConditionLinux()
   : pfd{-1, -1}
{
   // Create both nonblocking descriptors atomically. On failure no usable
   // condition exists, so propagate the error instead of using invalid fds.
   if (pipe2(this->pfd, O_NONBLOCK | O_CLOEXEC) < 0)
      throw std::system_error(errno, std::generic_category(), "Unable to create wait-condition pipe");
}

WaitConditionLinux::~WaitConditionLinux()
{
   close(this->pfd[0]);
   close(this->pfd[1]);
}

void WaitConditionLinux::release()
{
   L_DEBU(QString("WaitConditionLinux::release: begin write in pipe for read in fd=%1").arg(this->pfd[0]));
   ssize_t written;
   do
   {
      written = write(this->pfd[1], "", 1);
   }
   while (written < 0 && errno == EINTR);
   // A full pipe already represents a pending release. No userspace flag is
   // needed, and concurrent producers never block waiting for the consumer.
   if (written < 0 && errno != EAGAIN && errno != EWOULDBLOCK)
      L_ERRO("WaitConditionLinux::release: unable to signal wait-condition pipe.");
   L_DEBU(QString("WaitConditionLinux::release: end write in pipe for read in fd=%1").arg(this->pfd[0]));
}

bool WaitConditionLinux::wait(int timeout)
{
   // poll() supports descriptors above FD_SETSIZE and uses milliseconds,
   // including -1 for an indefinite wait, just like this method's API.
   pollfd fd{this->pfd[0], POLLIN, 0};
   L_DEBU(QString("WaitConditionLinux::wait: active poll for fd=%1").arg(this->pfd[0]));
   QDeadlineTimer deadline(timeout);
   int ready;
   do
   {
      ready = poll(&fd, 1, static_cast<int>(deadline.remainingTime()));
   }
   while (ready < 0 && errno == EINTR);
   if (ready == 0)
      return true;

   if (ready > 0 && (fd.revents & POLLIN))
   {
      L_DEBU(QString("WaitConditionLinux::wait: exit poll by release (fd=%1)").arg(this->pfd[0]));
      char dummy[4096];
      ssize_t bytesRead;
      do
      {
         bytesRead = read(this->pfd[0], dummy, sizeof(dummy));
      }
      while (bytesRead > 0 || (bytesRead < 0 && errno == EINTR));
   }
   else
      L_ERRO("WaitConditionLinux::wait: poll failed or woke without readable data.");

   return false;
}

int WaitConditionLinux::getFd()
{
   return this->pfd[0];
}
