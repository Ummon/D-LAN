#pragma once

#include <QMap>
#include <QMutex>
#include <QString>

namespace StressTests
{
   /**
     * Keeps the whole stress test directory under a given size. Shared by all the CoreSupervisors (thread-safe).
     *
     * The used size is estimated as:
     *  - the size measured on the disk (see 'measure(..)'), periodically updated by the main thread
     *  - + the files being written (reserved)
     *  - + the files written since the last measure (committed)
     *  - + the remaining bytes of the queued downloads of each Core
     */
   class DiskBudget
   {
   public:
      DiskBudget(qint64 maxBytes, int numberOfCores);

      qint64 getMaxBytes() const;
      qint64 getUsedBytes() const;

      /**
        * Reserve some space before writing a file.
        * @return false if there is not enough space left, in this case nothing is reserved.
        */
      bool tryReserve(qint64 bytes);

      /**
        * The reserved bytes won't be written, for example because of an error.
        */
      void release(qint64 bytes);

      /**
        * The reserved bytes have been written, they are kept until the next measure includes them.
        */
      void commit(qint64 bytes);

      /**
        * For the downloads: no reservation is done, the download queue of each Core is taken into account through
        * 'setPendingDownloadBytes(..)'.
        */
      bool canAfford(qint64 bytes) const;
      void setPendingDownloadBytes(int coreNumber, qint64 bytes);

      /**
        * The measure is done in three steps to not hold the lock while walking the directory tree:
        * 'beginMeasure()' -> 'measure(..)' -> 'endMeasure(..)'.
        */
      int beginMeasure();
      static qint64 measure(const QString& directory);
      void endMeasure(int generation, qint64 measuredBytes);

   private:
      qint64 getUsedBytesUnlocked() const;

      const qint64 maxBytes;

      mutable QMutex mutex;
      qint64 measuredBytes = 0;
      qint64 reservedBytes = 0;
      int generation = 0;
      QMap<int, qint64> committedBytes; ///< Generation -> bytes, see 'endMeasure(..)'.
      QList<qint64> pendingDownloadBytes; ///< One per Core.
   };
}
