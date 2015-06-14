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
  
#include <TransferRateCalculator.h>
using namespace Common;

#include <cstring>

#include <QMutexLocker>

/**
  * @class Common::TransferRateCalculator
  *
  * Compute an average value for a transfer rate in byte/s.
  * The period value is set in the header: PERIOD.
  * When some data are received or sent the method 'addData(..)' is called with the amount of data in bytes.
  * The current transfer rate can be retreived with the method 'getTransferRate()'.
  * An instance of 'TransferRateCalculator' can be shared among several threads.
  */

TransferRateCalculator::TransferRateCalculator() :
   mutex(QMutex::Recursive)
{
   this->reset();
}

void TransferRateCalculator::addData(int bytes)
{
   QMutexLocker locker(&this->mutex);

   if (bytes > 0)
      this->update(bytes);
}

/**
  * @return Rate in [B/s].
  */
int TransferRateCalculator::getTransferRate()
{
   QMutexLocker locker(&this->mutex);

   this->update(0);
   return this->total / PERIOD_S;
}

void TransferRateCalculator::reset()
{
   QMutexLocker locker(&this->mutex);

   this->currentValue = 0;
   this->currentValuePos = 0;
   this->total = 0;
   this->t1 = 0;
   memset(this->values, 0, NB_VALUE * sizeof(quint32));
   this->timer.start();
}

void TransferRateCalculator::update(int value)
{
   const qint64 t2 = this->timer.nsecsElapsed();
   forever
   {
      const qint64 dt = t2 - this->t1;
      const qint64 t1_ = this->t1 % D; // (t1 prime), relative to the current position.
      const qint64 t2_ = t1_ + dt; // (t2 prime), relative to t1 prime.

      if (t2_ < D) // We are in the current position.
      {
         this->currentValue += value;
         this->t1 = t2;
         return;
      }
      else if (dt > PERIOD)
      {
         if (value != 0)
         {
            this->currentValue = 0;
            this->currentValuePos = 0;
            this->t1 = 0;
            this->total = 0;
            const quint32 v = value / NB_VALUE;
            for (quint32 i = 0; i < NB_VALUE; i++)
            {
               this->values[i] = v;
               this->total += v;
            }
            this->timer.start();
         }
         else
            this->reset();

         return;
      }
      else
      {
         const quint32 v1 = qint64(value) * (D - t1_) / dt;

         this->total -= this->values[this->currentValuePos];
         this->values[this->currentValuePos++] = this->currentValue + v1;
         this->total += this->currentValue + v1;
         if (this->currentValuePos == NB_VALUE)
            this->currentValuePos = 0;

         value -= v1;
         this->t1 += (D - t1_);
         this->currentValue = 0;
      }
   }
}

