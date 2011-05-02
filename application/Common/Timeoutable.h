/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#ifndef COMMON_TIMEOUTABLE_H
#define COMMON_TIMEOUTABLE_H

#include <QObject>
#include <QTimer>

namespace Common
{
   // An inteface 'ITimeoutable' should be great but the QObject system doesn't support diamond inheritance.
   class Timeoutable : public QObject
   {
      Q_OBJECT
   protected:
      Timeoutable(int time);

   public:
      virtual ~Timeoutable() {}
      bool isTimedout() const;

   signals:
      void timeout();

   protected:
      virtual void startTimer();
      virtual void stopTimer();

   private slots:
      void timeoutSlot();

   private:
      bool timeouted;
      QTimer timer;
   };
}

#endif
