#include <DiskBudget.h>
using namespace StressTests;

#include <QDirIterator>
#include <QFileInfo>
#include <QMutexLocker>

DiskBudget::DiskBudget(qint64 maxBytes, int numberOfCores) :
   maxBytes(maxBytes),
   pendingDownloadBytes(numberOfCores, 0)
{
}

qint64 DiskBudget::getMaxBytes() const
{
   return this->maxBytes;
}

qint64 DiskBudget::getUsedBytes() const
{
   QMutexLocker locker(&this->mutex);
   return this->getUsedBytesUnlocked();
}

bool DiskBudget::tryReserve(qint64 bytes)
{
   QMutexLocker locker(&this->mutex);
   if (this->getUsedBytesUnlocked() + bytes > this->maxBytes)
      return false;
   this->reservedBytes += bytes;
   return true;
}

void DiskBudget::release(qint64 bytes)
{
   QMutexLocker locker(&this->mutex);
   this->reservedBytes -= bytes;
}

void DiskBudget::commit(qint64 bytes)
{
   QMutexLocker locker(&this->mutex);
   this->reservedBytes -= bytes;
   this->committedBytes[this->generation] += bytes;
}

bool DiskBudget::canAfford(qint64 bytes) const
{
   QMutexLocker locker(&this->mutex);
   return this->getUsedBytesUnlocked() + bytes <= this->maxBytes;
}

void DiskBudget::setPendingDownloadBytes(int coreNumber, qint64 bytes)
{
   QMutexLocker locker(&this->mutex);
   if (coreNumber >= 0 && coreNumber < this->pendingDownloadBytes.size())
      this->pendingDownloadBytes[coreNumber] = bytes;
}

/**
  * @return The generation to give to 'endMeasure(..)'.
  */
int DiskBudget::beginMeasure()
{
   QMutexLocker locker(&this->mutex);
   return this->generation++;
}

/**
  * Sum of the sizes of all the files in the given directory and its sub-directories.
  */
qint64 DiskBudget::measure(const QString& directory)
{
   qint64 total = 0;
   QDirIterator i(directory, QDir::Files | QDir::Hidden | QDir::System | QDir::NoSymLinks, QDirIterator::Subdirectories);
   while (i.hasNext())
   {
      i.next();
      total += i.fileInfo().size();
   }
   return total;
}

/**
  * The files committed before the call to 'beginMeasure()' (generation <= 'generation') are included in 'measuredBytes'.
  * The files committed during the measure may or may not be included, they are kept until the next measure.
  */
void DiskBudget::endMeasure(int generation, qint64 measuredBytes)
{
   QMutexLocker locker(&this->mutex);
   this->measuredBytes = measuredBytes;
   for (auto i = this->committedBytes.begin(); i != this->committedBytes.end();)
   {
      if (i.key() <= generation)
         i = this->committedBytes.erase(i);
      else
         ++i;
   }
}

qint64 DiskBudget::getUsedBytesUnlocked() const
{
   qint64 used = this->measuredBytes + this->reservedBytes;
   for (qint64 bytes : this->committedBytes)
      used += bytes;
   for (qint64 bytes : this->pendingDownloadBytes)
      used += bytes;
   return used;
}
