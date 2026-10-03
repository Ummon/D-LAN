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

#include <CoreService.h>
using namespace CoreSpace;

#include <QObject>
#include <QThread>

#include <Common/Constants.h>

CoreService::CoreService(bool resetSettings, QLocale locale, quint16 remoteControlPort, int argc, char** argv) :
#ifdef Q_OS_LINUX
   QtService<CoreApplication>(argc, argv, Common::Constants::SYSTEMD_UNIT_NAME),
#else
   QtService<CoreApplication>(argc, argv, Common::Constants::SERVICE_NAME),
#endif
   core(new Core(resetSettings, locale, remoteControlPort)),
   consoleReader(nullptr)
{
   this->setServiceDescription(tr("A LAN file sharing system"));
   this->setStartupType(QtServiceController::ManualStartup);
   this->setServiceFlags(QtServiceBase::Default);
}

CoreService::~CoreService()
{
   delete this->consoleReader; // 'delete nullptr' is a no-op.

   delete this->core;
   this->core = nullptr;
}

void CoreService::changePassword(const QString& newPassword)
{
   this->core->changePassword(newPassword);
}

void CoreService::removePassword()
{
   this->core->removePassword();
}

void CoreService::start()
{
   this->core->start();
}

void CoreService::stop()
{
   delete this->core;
   this->core = nullptr;

   this->application()->quit();
}

int CoreService::executeApplication()
{
   // If Core is launched as a regular application we read user input.
   if (!this->isRunningAsService())
   {
      QTextStream out(stdout);
      out << "D-LAN Core started with console support" << Qt::endl;
      CoreService::printCommands();

      this->consoleReader = new Common::ConsoleReader(this);
      connect(this->consoleReader, &Common::ConsoleReader::newLine, this, &CoreService::processUserInput, Qt::QueuedConnection);
   }

   return QtService::executeApplication();
}

void CoreService::processUserInput(QString input)
{
   if (input == "help")
   {
      this->printCommands();
   }
   else if (input == "quit")
   {
      this->stop();
   }
   else if (input == "dumpwi")
   {
      this->core->dumpWordIndex();
   }
   else if (input == "printsf")
   {
      this->core->printSimilarFiles();
   }
   else
   {
      QTextStream out(stdout);
      out << "Command unknown: '" << input << "', type 'help' to list commands" << Qt::endl;
   }
}

void CoreService::printCommands()
{
   QTextStream out(stdout);
   out << "Commands:" << Qt::endl
      << " - help: show this message" << Qt::endl
      << " - quit: stop the core" << Qt::endl
      << " - dumpwi: dump the word index in the log as a warning" << Qt::endl
      << " - printsf: print the similar files in the log as a warning" << Qt::endl;
}
