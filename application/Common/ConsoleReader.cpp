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
  
#include <Common/ConsoleReader.h>
using namespace Common;

#ifdef Q_OS_WIN32
   #include <windows.h>
#endif

/**
  * @class ConsoleReader
  *
  * @remarks If the executable is used as a sub-process and the parent-process is killed the signal
  * 'newLine(..)' is emitted with the 'QUIT_COMMAND'.
  */

QString ConsoleReader::QUIT_COMMAND("quit");

ConsoleReader::ConsoleReader(QObject *parent) :
    QThread(parent), inputStream(stdin), stopping(false)
{
}

void ConsoleReader::setQuitCommand(const QString& quitCommand)
{
   ConsoleReader::QUIT_COMMAND = quitCommand;
}

void ConsoleReader::stop()
{
   this->stopping = true;

   // TODO: Don't know how to unblock 'readLine' in 'ConsoleReader::run()' which use 'fgets(..)' internaly...
   // fclose(stdin); // Don't work, blocks.
   /*QIODevice* in = this->inputStream.device();
   close(((QFile*)in)->handle()); // Don't work.*/

   this->wait();
}

void ConsoleReader::run()
{
   while(!this->stopping)
   {
      // To send a 'QUIT_COMMAND' command if the stdin is closed (for example in a process -> child process case).
      if (std::feof(stdin))
      {
         emit newLine(QUIT_COMMAND);
         this->stopping = true;
         return;
      }

      QString str = this->inputStream.device()->readLine().trimmed();

      if (str.size() > 0)
         emit newLine(str);

      if (str == QUIT_COMMAND) // Cheating, see the 'stop()' method above.
         this->stopping = true;
   }
}
