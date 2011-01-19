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
  
#include <TransferRateCalculator.h>
using namespace Common;

#include <cstring>

#include <QMutexLocker>

TransferRateCalculator::TransferRateCalculator() :
   mutex(QMutex::Recursive), currentValue(0), currentValuePos(0), total(0)
{
   this->reset();
}

void TransferRateCalculator::addData(int bytes)
{
   QMutexLocker locker(&this->mutex);

   this->update(bytes);
}

/**
  * @return Rate in [B/s].
  */
int TransferRateCalculator::getTransferRate()
{
   QMutexLocker locker(&this->mutex);

   this->update(0);
   return 1000LL * this->total / PERIOD;
}

void TransferRateCalculator::reset()
{
   QMutexLocker locker(&this->mutex);

   this->currentValue = 0;
   this->currentValuePos = 0;
   this->total = 0;
   memset(this->values, 0, sizeof(this->values));
   this->timer.start();
}

void TransferRateCalculator::update(int value)
{
   const qint64 ELAPSED = this->timer.elapsed();

   if (ELAPSED < DELTA_T) // (we are in the current delta)
   {
      this->currentValue += value;
   }
   else if (ELAPSED >= PERIOD)
   {
      this->reset();
      this->currentValue = value;
   }
   else
   {
      const int N = ELAPSED / DELTA_T;

      this->total -= this->values[this->currentValuePos];
      this->values[this->currentValuePos] = this->currentValue;
      this->total += this->currentValue;
      this->stepForwardCurrentValuePos();

      for (int i = 0; i < N - 1; i++)
      {
         this->total -= this->values[this->currentValuePos];
         this->values[this->currentValuePos] = 0;

         this->stepForwardCurrentValuePos();
      }

      this->currentValue = value;
      this->timer.start();
   }
}

