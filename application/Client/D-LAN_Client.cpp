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

#include <D-LAN_Client.h>
using namespace Client;

#include <QFile>
#include <QStringList>

#include <Common/StringUtils.h>

#include <Log.h>

/**
  * @class GUI::D_LAN_Client
  *
  * Goals:
  *  - Connect to a existing core or to automatically launch one.
  *  - Control a core via simple commands or javascript files.
  */

D_LAN_Client::D_LAN_Client(int argc, char* argv[]) :
   QCoreApplication(argc, argv),
   out(stdout)
{
   this->out << "D-LAN Client" << endl;

   QScriptValue objectValue = this->engine.newQObject(this);
   this->engine.globalObject().setProperty("dlan", objectValue);

   this->parseApplicationArguments();

   connect(&this->consoleReader, &Common::ConsoleReader::newLine, this, &D_LAN_Client::newCommandLine, Qt::QueuedConnection);
}

QScriptValue D_LAN_Client::newConnection()
{
   CoreConnectionProxy* coreConnection = new CoreConnectionProxy();
   if (!this->coreExecutableDirectory.isNull())
      coreConnection->setCoreExecutableDirectory(this->coreExecutableDirectory);
   return this->engine.newQObject(coreConnection, QScriptEngine::ScriptOwnership);
}

void D_LAN_Client::print(QScriptValue v)
{
   this->out << v.toString() << endl;
}

void D_LAN_Client::newCommandLine(QString line)
{
   const QStringList& args = Common::StringUtils::splitArguments(line);

   if (args.isEmpty())
      return;

   if (args[0] == "quit")
   {
      this->quit();
   }
   else if (args[0] == "help")
   {
      this->printHelp();
   }
   else if (args[0] == "run")
   {
      if (args.length() < 2)
      {
         this->out << "You must provide a script file" << endl;
         return;
      }

      const QString& scriptName = args[1];
      QFile script((this->scriptDirectory.isEmpty() ? "" : this->scriptDirectory + '/') + scriptName);

      if (script.open(QIODevice::ReadOnly))
      {
         QScriptValue value = this->engine.evaluate(script.readAll());
         this->engine.collectGarbage();

         if (value.isError())
            this->out << "Script error: " << value.toString() << endl;
         else
            this->out << "Script output: " << value.toString() << endl;
      }
      else
         this->out << "Unable to open the script file" << endl;
   }
   else
   {
      this->out << "Unkown command, type help for more information" << endl;
   }
}

void D_LAN_Client::parseApplicationArguments()
{
   const QStringList& args = this->arguments();
   for (int i = 0; i < args.length(); ++i)
   {
      if (args[i] == "--core-dir" && i < args.length() - 1)
      {
         this->coreExecutableDirectory = args[i+1];
         L_DEBU(QString("Local core directory: %1").arg(this->coreExecutableDirectory));
      }
      else if (args[i] == "--script-dir" && i < args.length() - 1)
      {
         this->scriptDirectory = args[i+1];
         L_DEBU(QString("Script directory: %1").arg(this->scriptDirectory));
      }
   }
}

void D_LAN_Client::printHelp()
{
   this->out << this->applicationName() << " [--core-dir <directory>] [--script-dir <directory>] <commands>" << endl;
   this->out << "Options: " << endl <<
                " --core-dir: directory containing the core executable. Used only when starting local core" << endl <<
                " --script-dir: directory containing the script to execute with the 'run' command. If not given the working directory is used" << endl;
   this->out << "Commands:" << endl <<
                " - quit: quit the client" << endl <<
                " - help: print this help " << endl <<
                " - run <script name>: run a script name" << endl;
}
