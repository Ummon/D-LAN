/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#include <priv/StdLogger.h>
using namespace LM;

#if defined( Q_OS_WIN32 )
   #include <fcntl.h>
#endif

#include <IEntry.h>

/**
  * @class LM::StdLogger
  *
  * A special logger created to handle all stdout or stderr text line and log them.
  * Disable for the moment..
  */

// const StdLogger StdLogger::stdoutLogger(STDOUT_FILENO, "stdout");
// const StdLogger StdLogger::stderrLogger(STDERR_FILENO, "stderr");

/**
  * Fake class method to avoid the case where this compilation unit (.o)
  * is dropped by the linker when using 'libLogManager.a'.
  */
void StdLogger::init()
{
}

void StdLogger::run()
{
   qint64 size;
   while((size = this->stdoutIn.readLine(this->buffer, BUFFER_SIZE)) != -1)
      this->log(QString::fromAscii(this->buffer, size), SV_DEBUG);
}

StdLogger::StdLogger(int channel, const QString& name) :
   Logger(name), channel(channel)
{
   #if defined( Q_OS_WIN32 )
      _pipe(this->input, BUFFER_SIZE, O_TEXT);
   #elif defined( Q_OS_LINUX )
      pipe(this->input);
   #endif

   //this->channel = dup(this->channel);
   dup2(this->input[1], this->channel);
   close(this->input[1]);

   this->stdoutIn.open(this->input[0], QIODevice::ReadOnly);

   this->start();
}

StdLogger::~StdLogger()
{
   close(this->channel);
   this->wait();
}
