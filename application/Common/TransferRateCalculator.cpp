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

#include <QMutexLocker>

TransferRateCalculator::TransferRateCalculator()
   : bytesTransmitted(0)
{
}

void TransferRateCalculator::addData(int bytes)
{
   QMutexLocker locker(&this->mutex);
   this->bytesTransmitted += bytes;
}

/**
  * @return Rate in [B/s].
  */
int TransferRateCalculator::getTransferRate() const
{
   QMutexLocker locker(&this->mutex);

   if (this->timer.elapsed() == 0)
      return 0;

   return this->bytesTransmitted / this->timer.elapsed() * 1000;
}

void TransferRateCalculator::reset()
{
   QMutexLocker locker(&this->mutex);
   this->timer.start();
   this->bytesTransmitted = 0;
}
