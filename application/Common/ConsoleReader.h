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
  
#ifndef COMMON_CONSOLEREADER_H
#define COMMON_CONSOLEREADER_H

#include <QThread>
#include <QTextStream>
#include <QIODevice>

namespace Common
{
   class ConsoleReader : public QThread
   {
      Q_OBJECT
   public:
      static QString QUIT_COMMAND;

      explicit ConsoleReader(QObject* parent = nullptr);

      static void setQuitCommand(const QString& quitCommand);

      void stop();

   protected:
      void run();

   signals:
      void newLine(QString line);

   private:
      QTextStream inputStream;
      bool stopping;
   };
}

#endif
