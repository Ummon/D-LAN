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

int TransferRateCalculator::getTransferRate() const
{
   QMutexLocker locker(&this->mutex);

   if (this->timer.elapsed() == 0)
      return 0;

   return this->bytesTransmitted / this->timer.elapsed();
}

void TransferRateCalculator::reset()
{
   QMutexLocker locker(&this->mutex);
   this->timer.start();
   this->bytesTransmitted = 0;
}
