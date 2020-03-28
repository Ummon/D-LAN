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
  
#pragma once

#include <QObject>
#include <QThread>
#include <QFile>
#include <QTextStream>

#include <priv/Logger.h>

namespace LM
{
   class StdLogger : public QThread, public Logger
   {
   private:
      static const int BUFFER_SIZE = 512;

   public:
      static const StdLogger stdoutLogger;
      static const StdLogger stderrLogger;
      static void init();

   protected:
      void run();

   private:
       StdLogger(int file, const QString& name);
       virtual ~StdLogger();

   private:
      int channel;
      int input[2];
      QFile stdoutIn;
      char buffer[BUFFER_SIZE];
   };
}
