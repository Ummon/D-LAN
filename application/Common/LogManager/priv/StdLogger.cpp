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
  
#include <priv/StdLogger.h>
using namespace LM;

#include <IEntry.h>

/**
  * @class StdLogger
  * A special logger created to handle all stdout or stderr text line and log them.
  * TODO: It doesn't work because SIGNAL(readyRead()) can't be apply to a QFile.
  *  see : http://lists.trolltech.com/qt-interest/2004-12/thread00816-0.html
  */

const StdLogger StdLogger::stdoutLogger(stdout, "stdout");
const StdLogger StdLogger::stderrLogger(stderr, "stderr");

/**
  * Fake class method to avoid the case where this compilation unit (.o)
  * is dropped by the linker when using 'libLogManager.a'.
  */
void StdLogger::init()
{
}

#if defined( Q_OS_WIN )
   #include <fcntl.h>
#endif

StdLogger::StdLogger(FILE* file, const QString& name) :
   Logger(name)
{
   /*#if defined( Q_OS_WIN )
      _pipe(this->input, 0x1000, O_TEXT);
   #else
      pipe(this->input);
   #endif

   dup2(input[STDOUT_FILENO], STDOUT_FILENO);
   close(input[STDOUT_FILENO]);

   stdoutIn.open(input[STDIN_FILENO], QIODevice::ReadOnly);*/

   stdoutIn.open(file, QIODevice::ReadOnly);

   connect(&this->stdoutIn, SIGNAL(readyRead()), this, SLOT(newData()));
}

void StdLogger::newData()
{
   if (this->stdoutIn.canReadLine())
   {
      this->stdoutIn.readLine(this->buffer, BUFFER_SIZE);
      this->log(this->buffer, SV_DEBUG);
   }
}
