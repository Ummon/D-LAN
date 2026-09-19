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
  
#include <priv/FileUpdater/WaitConditionDarwin.h>
using namespace FM;

#include <QDeadlineTimer>
#include <QDebug>
#include <cerrno>
#include <fcntl.h>
#include <poll.h>
#include <system_error>
#include <unistd.h>

WaitConditionDarwin::WaitConditionDarwin() : pfd{-1, -1}
{
   if (pipe(this->pfd) < 0)
      throw std::system_error(errno, std::generic_category(), "Unable to create wait-condition pipe");
   // Darwin has no pipe2(). Both ends must be nonblocking and not inherited
   // by subprocesses. Close both descriptors on partial initialization failure.
   for (int fd : this->pfd)
      if (fcntl(fd, F_SETFL, O_NONBLOCK) < 0 || fcntl(fd, F_SETFD, FD_CLOEXEC) < 0)
      {
         const int error = errno;
         close(this->pfd[0]);
         close(this->pfd[1]);
         throw std::system_error(error, std::generic_category(), "Unable to configure wait-condition pipe");
      }
}

WaitConditionDarwin::~WaitConditionDarwin()
{
   close(this->pfd[0]);
   close(this->pfd[1]);
}

void WaitConditionDarwin::release()
{
   ssize_t written;
   do
      written = write(this->pfd[1], "", 1);
   while (written < 0 && errno == EINTR);
   // A full pipe already represents a pending release.
   if (written < 0 && errno != EAGAIN && errno != EWOULDBLOCK)
      qWarning("Unable to signal Darwin wait-condition pipe");
}

bool WaitConditionDarwin::wait(int timeout)
{
   pollfd fd{this->pfd[0], POLLIN, 0};
   QDeadlineTimer deadline(timeout);
   int ready;
   do
      ready = poll(&fd, 1, static_cast<int>(deadline.remainingTime()));
   while (ready < 0 && errno == EINTR);
   if (ready == 0)
      return true;
   if (ready > 0 && (fd.revents & POLLIN))
   {
      char buffer[4096];
      ssize_t count;
      do
         count = read(this->pfd[0], buffer, sizeof(buffer));
      while (count > 0 || (count < 0 && errno == EINTR));
   }
   else
      qWarning("Unable to poll Darwin wait-condition pipe");
   return false;
}

int WaitConditionDarwin::getFd() const
{
   return this->pfd[0];
}
