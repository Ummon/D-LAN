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
  * The current transfer rate can be retrieved with the method 'getTransferRate()'.
  * An instance of 'TransferRateCalculator' can be shared among several threads.
  *
  * It also keeps a history of the transfer rate, one sample per second for the last NB_SAVED_AVERAGED_RATE_SAMPLES seconds,
  * see 'getTransferRateAt(..)' and 'getTransferRatesRange(..)'.
  */

TransferRateCalculator::TransferRateCalculator()
{
   this->reset();
   memset(this->transferRateHistory, 0, sizeof(this->transferRateHistory));
   this->timerTransferRateHistory.start();
}

void TransferRateCalculator::addData(qint64 bytes)
{
   QMutexLocker locker(&this->mutex);

   if (bytes > 0)
      this->update(static_cast<quint64>(bytes));
}

/**
  * @return Rate in [B/s].
  */
quint64 TransferRateCalculator::getTransferRate()
{
   QMutexLocker locker(&this->mutex);   
   this->update(0);

   return this->computeTransferRate();
}

/**
  * @param n Age of the sample in seconds: 0 is the current second (its value can still change), 1 the previous one, etc.
  * @return Rate in [B/s] at the given second, 0 if it is older than the history or than this object.
  */
quint64 TransferRateCalculator::getTransferRateAt(quint64 n)
{
   QMutexLocker locker(&this->mutex);
   this->update(0);

   return this->getSavedTransferRate(n);
}

/**
  * @return The rates in [B/s] of 'nb' seconds, the newest first: element i is 'getTransferRateAt(from + i)'.
  */
QList<quint64> TransferRateCalculator::getTransferRatesRange(quint64 from, int nb)
{
   QMutexLocker locker(&this->mutex);
   this->update(0);

   QList<quint64> result;
   if (nb <= 0)
      return result;

   result.reserve(nb);
   for (int i = 0; i < nb; i++)
      result << this->getSavedTransferRate(from + i);
   return result;
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

void TransferRateCalculator::update(quint64 value)
{
   const qint64 t2 = this->timer.nsecsElapsed();
   if (t2 - this->t1 > PERIOD)
   {
      this->reset();
      this->currentValue = value;
      this->updateTransferRateHistory();
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

   this->updateTransferRateHistory();
}

void TransferRateCalculator::updateTransferRateHistory()
{
   const quint64 currentSec = this->timerTransferRateHistory.elapsed() / 1000;

   // The seconds between the last saved one and the current one had no update: their rate is set to 0.
   if (currentSec - this->transferRateHistoryLastSec >= NB_SAVED_AVERAGED_RATE_SAMPLES)
      memset(this->transferRateHistory, 0, sizeof(this->transferRateHistory));
   else
      for (quint64 sec = this->transferRateHistoryLastSec + 1; sec < currentSec; sec++)
         this->transferRateHistory[sec % NB_SAVED_AVERAGED_RATE_SAMPLES] = 0;

   this->transferRateHistoryLastSec = currentSec;
   this->transferRateHistory[currentSec % NB_SAVED_AVERAGED_RATE_SAMPLES] = this->computeTransferRate();
}

quint64 TransferRateCalculator::getSavedTransferRate(quint64 n) const
{
   if (n >= NB_SAVED_AVERAGED_RATE_SAMPLES || n > this->transferRateHistoryLastSec)
      return 0;

   return this->transferRateHistory[(this->transferRateHistoryLastSec - n) % NB_SAVED_AVERAGED_RATE_SAMPLES];
}
