#ifndef FILEMANAGER_DIRWATCHER_H
#define FILEMANAGER_DIRWATCHER_H

#include <QtCore/QList>
#include <QtCore/QString>

namespace FM
{
   class DirWatcherException : public std::exception
   {
      public :
      DirWatcherException(const QString& mess) : mess(mess) { }
      virtual ~DirWatcherException() throw () { }
      const QString mess;
   };

   struct WatcherEvent;

   /**
     * An abstract directory watcher.
     * Can watch several directories recursively.
     *
     * There must be an implementation for the current platform,
     * if any exist, an error will occur during compilation.
     * See the factory 'getNewWatcher'.
     *
     * Event types :
     *  - Rename dir
     *  - Rename file
     *  - New file
     *  - Delete file
     *  - The content of a file changed
     */
   class DirWatcher
   {
   protected:
      DirWatcher();
   public:
      virtual ~DirWatcher();

      /**
        * Build a new watcher.
        * The implementation depends of the platform.
        * @return 0 if there is no implementation for the current platform.
        */
      static DirWatcher* getNewWatcher();

      /**
        * Add a directory to watch.
        * Each new added directory is immediately watched. If some modification
        * occurs in the file system bewteen a call of 'addDir' and a call of 'waitEvent' they
        * will be recorded and the next call to 'waitEvent' will be no blocking.
        */
      virtual void addDir(const QString& path) = 0;

      /**
        * Remove a watched directory.
        */
      virtual void rmDir(const QString& path) = 0;

      /**
        * Return the number of watched directory.
        */
      virtual int nbWatchedDir() = 0;

      /**
        * Wait a new event from the listened directories.
        * There is no timeout, it can wait forever.
        */
      virtual const QList<WatcherEvent> waitEvent() = 0;

      /**
        * Wait a new event from the listened directories.
        * @param timeout A timeout in milliseconds.
        */
      virtual const QList<WatcherEvent> waitEvent(int timeout) = 0;
   };

   /**
     * When a event occurs this struct is returned.
     */
   struct WatcherEvent
   {
      enum Type {
         RENAME_DIR,
         RENAME_FILE,
         NEW_FILE,
         DELETE_FILE,
         CONTENT_FILE_CHANGED,
         TIMEOUT,
         UNKNOWN
      };

      WatcherEvent();
      WatcherEvent(Type type);
      WatcherEvent(Type type, const QString& path1);
      WatcherEvent(Type type, const QString& path1, const QString& path2);

      /**
        * Default assignment operator does nothing because all members are const.
        */
      WatcherEvent& operator=(const WatcherEvent&) { return *this; }

      const Type type;
      const QString path1;
      const QString path2; // Only used with type 'RENAME_*'.
   };
}

#endif
