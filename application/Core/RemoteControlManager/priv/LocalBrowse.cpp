#include <priv/LocalBrowse.h>

#include <QDateTime>
#include <QDirIterator>
#include <QPromise>
#include <QSharedPointer>
#include <QStorageInfo>
#include <stdexcept>

namespace
{
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
   static QThreadPool pool;
   static const bool configured = [] { pool.setMaxThreadCount(2); return true; }();
   Q_UNUSED(configured)
   return pool;
}

QFuture<Protos::GUI::LocalBrowseResult> RCM::localBrowse(const Protos::GUI::LocalBrowse& request)
{
   auto promise = QSharedPointer<QPromise<Protos::GUI::LocalBrowseResult>>::create();
   promise->start();
   const auto future = promise->future();
   localBrowsePool().start([request, promise] {
      try
      {
         enumerate(request, *promise);
      }
      catch (...)
      {
         promise->setException(std::current_exception());
      }
      promise->finish();
   });
   return future;
}
