#ifndef COMMON_TRANSFERRATECALCULATOR_H
#define COMMON_TRANSFERRATECALCULATOR_H

#include <QMutex>
#include <QElapsedTimer>

namespace Common
{
   class TransferRateCalculator
   {
   public:
      TransferRateCalculator();

      void addData(int bytes);
      int getTransferRate() const;
      void reset();

   private:
      mutable QMutex mutex;
      QElapsedTimer timer;
      int bytesTransmitted;
   };
}

#endif
