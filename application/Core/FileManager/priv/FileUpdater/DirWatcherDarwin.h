#pragma once

#include <priv/FileUpdater/DirWatcher.h>
#include <priv/FileUpdater/WaitConditionDarwin.h>

#include <CoreServices/CoreServices.h>
#include <dispatch/dispatch.h>
#include <QMutex>
#include <map>
#include <memory>

class DirWatcherDarwinTests;

namespace FM
{
   // FSEvents runs on a serial dispatch queue, independent of Qt's event loop.
   // Registrations may be changed while the updater thread is in waitEvent().
   class DirWatcherDarwin : public DirWatcher
   {
   public:
      DirWatcherDarwin();
      ~DirWatcherDarwin() override;
      bool addPath(const QString& path, const QString& filename = QString()) override;
      void rmPath(const QString& path, const QString& filename = QString()) override;
      int nbWatchedPath() override;
      const QList<WatcherEvent> waitEvent(QList<WaitCondition*> ws = {}) override;
      const QList<WatcherEvent> waitEvent(int timeout, QList<WaitCondition*> ws = {}) override;

   private:
      friend class ::DirWatcherDarwinTests;
      struct Watch;
      static void callback(ConstFSEventStreamRef, void*, size_t, void*,
         const FSEventStreamEventFlags[], const FSEventStreamEventId[]);
      static void recordEvents(Watch&, size_t, char* const*, const FSEventStreamEventFlags*);
      QList<WatcherEvent> takeEvents();

      WaitConditionDarwin changed;
      dispatch_queue_t queue;
      QMutex mutex;
      std::map<QString, std::unique_ptr<Watch>> watches;
   };
}
