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
  
#ifndef CLIENT_D_LAN_CLIENT_H
#define CLIENT_D_LAN_CLIENT_H

#include <QCoreApplication>
#include <QScriptEngine>
#include <QList>
#include <QTextStream>

#include <Common/ConsoleReader.h>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <CoreConnectionProxy.h>

namespace Client
{
   class D_LAN_Client : public QCoreApplication
   {
      Q_OBJECT
   public:
      D_LAN_Client(int argc, char* argv[]);

   public slots:
      QScriptValue newConnection();

   private slots:
      void newCommandLine(QString line);

   private:
      void printHelp();

      QScriptEngine engine;

      QTextStream out;
      Common::ConsoleReader consoleReader;
   };
}

#endif
