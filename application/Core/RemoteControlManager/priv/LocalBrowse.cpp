#include <priv/LocalBrowse.h>

#include <QDateTime>
#include <QDirIterator>
#include <QPromise>
#include <QSharedPointer>
#include <QStorageInfo>
#include <stdexcept>
#include <QMutex>
#include <QMutexLocker>
#include <QRunnable>

namespace
{
   struct BrowsePool
   {
      QMutex mutex;
      int queued = 0;
      // Destroy the pool before the mutex: running jobs use the mutex when starting.
      QThreadPool pool;
      BrowsePool() { pool.setMaxThreadCount(2); }
   };

   BrowsePool& browsePool()
   {
      static BrowsePool state;
      return state;
   }

   struct Job
   {
      QPromise<Protos::GUI::LocalBrowseResult> promise;
      QRunnable* queuedRunnable = nullptr; // Protected by BrowsePool::mutex.
   };

   constexpr QDir::Filters FILTERS = QDir::Dirs | QDir::Files | QDir::NoDotAndDotDot | QDir::Hidden;

   void enumerate(const Protos::GUI::LocalBrowse& request, QPromise<Protos::GUI::LocalBrowseResult>& promise)
   {
      if (promise.isCanceled())
         return;

      Protos::GUI::LocalBrowseResult result;
      quint64 resultSize = 32; // Tag and protobuf envelope overhead.
      const auto accountEntry = [&](const Protos::GUI::LocalBrowseResult::Entry& entry) {
         resultSize += entry.ByteSizeLong() + 10;
         // Match MessageSocket's receive limit; never send an unreadable, partial result.
         if (resultSize > 100 * 1024 * 1024)
            throw std::length_error("Local browse response exceeds the message size limit");
      };
      result.set_tag(request.tag());
      const QString path = QString::fromStdString(request.path());
      if (path.isEmpty())
      {
         for (const QStorageInfo& storage : QStorageInfo::mountedVolumes())
         {
            if (promise.isCanceled())
               return;
            if (!storage.isValid() || !storage.isReady())
               continue;
            auto* entry = result.add_entries();
            entry->set_name(storage.rootPath().toStdString());
            entry->set_type(Protos::GUI::LocalBrowseResult::DIR);
            entry->set_size(storage.bytesTotal() - storage.bytesAvailable());
            entry->set_volume_label(storage.name().toStdString());
            entry->set_capacity(storage.bytesTotal());
            accountEntry(*entry);
         }
      }
      else
      {
         QDirIterator entries(path, FILTERS);
         while (!promise.isCanceled() && entries.hasNext())
         {
            entries.next();
            const QFileInfo info = entries.fileInfo();
            const bool isDir = info.isDir();
            auto* entry = result.add_entries();
            entry->set_name(info.fileName().toStdString());
            entry->set_type(isDir ? Protos::GUI::LocalBrowseResult::DIR : Protos::GUI::LocalBrowseResult::FILE);
            entry->set_date_modified(info.lastModified().toMSecsSinceEpoch());
            if (isDir)
            {
               // Preserve the protocol's child count without allocating a second directory listing.
               qint64 count = 0;
               QDirIterator children(info.absoluteFilePath(), FILTERS);
               while (!promise.isCanceled() && children.hasNext())
               {
                  children.next();
                  ++count;
               }
               entry->set_size(count);
            }
            else
               entry->set_size(info.size());
            accountEntry(*entry);
         }
      }
      if (!promise.isCanceled())
         promise.addResult(std::move(result));
   }
}

QThreadPool& RCM::localBrowsePool()
{
   return browsePool().pool;
}

RCM::LocalBrowseJob RCM::localBrowse(const Protos::GUI::LocalBrowse& request)
{
   auto job = QSharedPointer<Job>::create();
   job->promise.start();
   const auto future = job->promise.future();
   auto cancel = [job] {
      job->promise.future().cancel();
      auto& state = browsePool();
      QMutexLocker lock(&state.mutex);
      // A running job clears this pointer before doing any work. Holding the mutex
      // prevents auto-deletion from racing with tryTake (and reusing the address).
      if (job->queuedRunnable && state.pool.tryTake(job->queuedRunnable))
      {
         delete job->queuedRunnable; // Release the captured request immediately.
         job->queuedRunnable = nullptr;
         --state.queued;
         job->promise.finish();
      }
   };
   auto& state = browsePool();
   QMutexLocker lock(&state.mutex);
   if (state.queued >= 40)
   {
      job->promise.setException(std::make_exception_ptr(std::runtime_error("Local browse queue is full")));
      job->promise.finish();
      return {future, cancel};
   }
   job->queuedRunnable = QRunnable::create([request, job] {
      {
         auto& state = browsePool();
         QMutexLocker lock(&state.mutex);
         job->queuedRunnable = nullptr;
         --state.queued;
      }
      try
      {
         enumerate(request, job->promise);
      }
      catch (...)
      {
         job->promise.setException(std::current_exception());
      }
      job->promise.finish();
   });
   ++state.queued;
   state.pool.start(job->queuedRunnable);
   return {future, cancel};
}
