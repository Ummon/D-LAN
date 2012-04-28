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
  
#ifndef COMMON_TRANSFERRATECALCULATOR_H
#define COMMON_TRANSFERRATECALCULATOR_H

#include <QMutex>
#include <QElapsedTimer>

#include <Common/Uncopyable.h>

namespace Common
{
   class TransferRateCalculator : Common::Uncopyable
   {
      static const quint32 PERIOD = 3000000000u; // [ns].
      static const quint32 NB_VALUE = 20;
      static const quint32 DELTA_T = PERIOD / NB_VALUE; // [ns].

   public:
      TransferRateCalculator();

      void addData(int bytes);
      int getTransferRate();

   private:
      void reset();
      void update(int value);
      inline void stepForwardCurrentValuePos();

      mutable QMutex mutex;
      QElapsedTimer timer;

      int currentValue;
      quint32 currentValuePos;
      int values[NB_VALUE]; // Previous values during the current period.

      int total; // Sum of all values.
   };
}

using namespace Common;

inline void TransferRateCalculator::stepForwardCurrentValuePos()
{
   this->currentValuePos++;
   if (this->currentValuePos >= NB_VALUE)
      this->currentValuePos = 0;
}
#endif
