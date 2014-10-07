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
#include <QStringList>
  
#include <D-LAN_Client.h>
using namespace Client;

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

   connect(&this->consoleReader, SIGNAL(newLine(QString)), this, SLOT(newCommandLine(QString)), Qt::QueuedConnection);
}

QJSValue D_LAN_Client::newConnection()
{
   return this->engine.newQObject(new CoreConnectionProxy());
}

void D_LAN_Client::print(QJSValue v)
{
   this->out << v.toString() << endl;
}

//Q_SCRIPT_DECLARE_QMETAOBJECT(QFile, QObject*)
//Q_SCRIPT_DECLARE_QMETAOBJECT(QIODevice, QObject*)

void D_LAN_Client::newCommandLine(QString line)
{
   if (line == "quit")
   {
      this->quit();
   }
   else if (line == "help")
   {
      this->printHelp();
   }
   else if (line == "run")
   {
      QJSValue objectValue = this->engine.newQObject(this);
      this->engine.globalObject().setProperty("dlan", objectValue);

      QFile script("../../test_script_1.js");
      if (script.open(QIODevice::ReadOnly))
      {
         QJSValue value = this->engine.evaluate(script.readAll());

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

void D_LAN_Client::printHelp()
{
   this->out << "Commands:" << endl <<
                " - quit : quit the client" << endl <<
                " - help : print this help " << endl <<
                " - run <script name> : run a script name" << endl;
}
