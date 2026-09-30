#pragma once

#include <memory>

#include <QDateTime>
#include <QList>
#include <QMap>
#include <QMutex>
#include <QString>
#include <QStringList>

#include <Common/Hash.h>

namespace StressTests
{
   /**
     * Shared by all the CoreSupervisors (thread-safe), for the search action:
     *  - Each supervisor publishes the peer ID and the shared entries of its Core, a searching supervisor uses them
     *    to pick a file of another Core and to recognize it in the search results.
     *  - A searched file is locked during the search: it can't be deleted (neither one of its parent directories)
     *    and its Core can't be restarted.
     */
   class SearchCoordinator
   {
   public:
      struct SharedEntry
      {
         Common::Hash id;
         QString path; ///< Absolute, a directory ends with '/'.
      };

      struct CoreInfo
      {
         Common::Hash peerID;
         QList<SharedEntry> sharedEntries;
         QDateTime availableSince; ///< Since when the Core is connected, it's reset when it's restarted.
      };

      /**
        * Released when destroyed. Must not outlive the SearchCoordinator.
        */
      class FileLock
      {
      public:
         ~FileLock();

      private:
         friend class SearchCoordinator;
         FileLock(SearchCoordinator& coordinator, quint64 id);

         SearchCoordinator& coordinator;
         const quint64 id;
      };

      explicit SearchCoordinator(int numberOfCores);

      /**
        * Called at each state received from the Core.
        */
      void setCoreAvailable(int coreNumber, const Common::Hash& peerID, const QList<SharedEntry>& sharedEntries);
      void setCoreUnavailable(int coreNumber);

      /**
        * @return false if the Core isn't available.
        */
      bool getCoreInfo(int coreNumber, CoreInfo& info) const;

      /**
        * @return nullptr if the file is being deleted or if its Core isn't available.
        */
      std::unique_ptr<FileLock> lockFile(int coreNumber, const QString& filepath);

      /**
        * To call before deleting a file or a directory, 'endDelete(..)' must be called after.
        * @return false if the entry is a locked file or contains a locked file, in this case it must not be deleted.
        */
      bool beginDelete(const QString& path);
      void endDelete(const QString& path);

      /**
        * To call before restarting a Core, the Core is unavailable until its next 'setCoreAvailable(..)'.
        * @return false if a file of this Core is locked, in this case it must not be restarted.
        */
      bool beginRestart(int coreNumber);

   private:
      void unlock(quint64 id);

      struct Lock
      {
         int coreNumber;
         QString filepath;
      };

      mutable QMutex mutex;
      QList<CoreInfo> cores; ///< A null 'availableSince' means unavailable.
      QMap<quint64, Lock> locks;
      quint64 nextLockID = 1;
      QStringList beingDeleted;
   };
}
