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

#include <QRecursiveMutex>
#include <QElapsedTimer>
#include <QList>

#include <Common/Uncopyable.h>

namespace Common
{
   class TransferRateCalculator : Common::Uncopyable
   {
      static const quint32 PERIOD = 3000000000u; // [ns]. (3 seconds).
      static const quint32 PERIOD_S = PERIOD / 1000000000; // [s].
      static const quint32 NB_VALUE = 30;
      static const quint32 D = PERIOD / NB_VALUE; // [ns].

      static const quint32 NB_SAVED_AVERAGED_RATE_SAMPLES = 20 * 60; // 20 min, 1 sample = 1 sec.

   public:
      TransferRateCalculator();

      void addData(qint64 bytes);
      quint64 getTransferRate();

      quint64 getTransferRateAt(quint64 n);
      QList<quint64> getTransferRatesRange(quint64 from, int nb);

   private:
      void reset();
      void update(quint64 value);
      void updateTransferRateHistory();
      quint64 getSavedTransferRate(quint64 n) const;

      inline quint64 computeTransferRate() const { return this->total / PERIOD_S; }

      mutable QRecursiveMutex mutex;
      QElapsedTimer timer;      
      qint64 t1;

      // 64 bits: a 'quint32' total overflows from ~1.4 GB/s over the period, which a loopback or a
      // fast local link can reach.
      quint64 currentValue;
      quint32 currentValuePos;
      quint64 values[NB_VALUE]; // Previous values during the current period.
      quint64 total; // Sum of all values.

      // History of transfer rates.
      QElapsedTimer timerTransferRateHistory;
      quint64 transferRateHistory[NB_SAVED_AVERAGED_RATE_SAMPLES]; // Circular buffer to save the last transfer rates.
      quint64 transferRateHistoryLastSec = 0;

   };
}
