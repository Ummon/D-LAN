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
  
#include <QFile>
  
#include <D-LAN_Client.h>
using namespace Client;

/**
  * @class GUI::D_LAN_Client
  *
  * Goals:
  *  - Connect to a existing core or to automatically launch one.
  *  - Control a core via simple commands or javascript files.
  */

D_LAN_Client::D_LAN_Client(int argc, char *argv[]) :
   QCoreApplication(argc, argv),
   out(stdout)
{
   this->out << "D-LAN Client" << endl;

   connect(&this->consoleReader, SIGNAL(newLine(QString)), this, SLOT(newCommandLine(QString)), Qt::QueuedConnection);
   this->consoleReader.start();
}

QScriptValue D_LAN_Client::newConnection()
{
   return this->engine.newQObject(new CoreConnectionProxy(), QScriptEngine::ScriptOwnership);
}

void D_LAN_Client::newCommandLine(QString line)
{
   if (line == ConsoleReader::QUIT_COMMAND)
   {
      this->quit();
      this->consoleReader.stop();
   }
   else if (line == "help")
   {
      this->printHelp();
   }
   else if (line == "run")
   {
      QScriptValue objectValue = this->engine.newQObject(this);
      this->engine.globalObject().setProperty("dlan", objectValue);

      QFile script("../../test_script_1.dlan");
      if (script.open(QIODevice::ReadOnly))
      {
         QScriptValue value = this->engine.evaluate(script.readAll());

         if (this->engine.hasUncaughtException())
            this->out << "Script error: " << value.toString() << endl;
      }
      else
         this->out << "Unable to open the script file" << endl;
   }
   else
   {
      this->out << "Unkown command, type help for more information" << endl;
   }
}

void D_LAN_Client::printHelp()
{
   this->out << "Commands:" << endl <<
                " - help : print this help " << endl <<
                " - run <script name> : run a script name" << endl;
}
