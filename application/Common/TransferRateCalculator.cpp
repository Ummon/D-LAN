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
#include <limits>

#include <QMutexLocker>

/**
  * @class Common::TransferRateCalculator
  *
  * Compute an average value for a transfer rate in byte/s.
  * The period value is set in the header: PERIOD.
  * When some data are received or sent the method 'addData(..)' is called with the amount of data in bytes.
  * The current transfer rate can be retrieved with the method 'getTransferRate()'.
  * An instance of 'TransferRateCalculator' can be shared among several threads.
  */

TransferRateCalculator::TransferRateCalculator()
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

   const quint64 rate = this->total / PERIOD_S;

   // The returned type is signed and 32 bits, the rate is saturated instead of being wrapped.
   return rate > static_cast<quint64>(std::numeric_limits<int>::max()) ?
      std::numeric_limits<int>::max() :
      static_cast<int>(rate);
}

void TransferRateCalculator::reset()
{
   QMutexLocker locker(&this->mutex);

   this->currentValue = 0;
   this->currentValuePos = 0;
   this->total = 0;
   this->t1 = 0;
   memset(this->values, 0, sizeof(this->values));
   this->timer.start();
}

void TransferRateCalculator::update(int value)
{
   const qint64 t2 = this->timer.nsecsElapsed();
   if (t2 - this->t1 > PERIOD)
   {
      this->reset();
      this->currentValue = value;
      return;
   }

   // Age existing data before recording the new bytes. They arrived now, so
   // they must not be spread over the time since the last add or rate query.
   while (this->t1 / D < t2 / D)
   {
      this->total -= this->values[this->currentValuePos];
      this->values[this->currentValuePos++] = this->currentValue;
      this->total += this->currentValue;
      if (this->currentValuePos == NB_VALUE)
         this->currentValuePos = 0;

      this->t1 += D - this->t1 % D;
      this->currentValue = 0;
   }

   this->currentValue += value;
   this->t1 = t2;
}
