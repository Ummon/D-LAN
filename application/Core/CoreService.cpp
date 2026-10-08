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

/**
  * @return 0 if 'value' isn't a valid port.
  */
quint16 CoreService::parsePort(const QString& value)
{
   bool ok = false;
   const uint port = value.toUInt(&ok);
   return ok && port <= 65535 ? static_cast<quint16>(port) : 0;
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

/**
  * The arguments given to a Windows service when it's started, by 'RCC::CoreController::startCore(..)' for instance,
  * aren't on the command line of the process read by 'main(..)': they are only known here.
  */
void CoreService::createApplication(int& argc, char** argv)
{
   if (this->isRunningAsService())
      for (int i = 1; i < argc - 1; i++)
         if (qstrcmp(argv[i], "--port") == 0)
         {
            const quint16 port = CoreService::parsePort(QString::fromLatin1(argv[++i]));
            if (port != 0)
               this->core->setRemoteControlPort(port);
            else
               L_WARN(QString("Invalid remote control port: %1").arg(QString::fromLatin1(argv[i])));
         }

   QtService::createApplication(argc, argv);
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
