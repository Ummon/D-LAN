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
  
#ifndef STDOUTLOGGER_H
#define STDOUTLOGGER_H

#include <QObject>
#include <QFile>

#include <priv/Logger.h>

namespace LM
{
   class StdLogger : public QObject, public Logger
   {
      Q_OBJECT
   private:
      static const int BUFFER_SIZE = 512;

   public:
      static const StdLogger stdoutLogger;
      static const StdLogger stderrLogger;
      static void init();

   private:
       StdLogger(FILE* file, const QString& name);

   private slots:
       void newData();

   private:
      int input[2];
      QFile stdoutIn;
      char buffer[BUFFER_SIZE];
   };
}

#endif
