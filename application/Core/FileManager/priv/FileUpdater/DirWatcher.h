/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef FILEMANAGER_DIRWATCHER_H
#define FILEMANAGER_DIRWATCHER_H

#include <QtCore/QList>
#include <QtCore/QString>

namespace FM
{
   class WaitCondition;
   struct WatcherEvent;

   /**
     * An abstract directory watcher.
     * Can watch several directories recursively.
     *
     * An implementation may or may not exist for the current platform,
     * see the factory 'getNewWatcher()'.
     *
     * Event types:
     *  - Rename dir
     *  - Rename file
     *  - New file
     *  - Delete file
     *  - The content of a file changed
     *
     * Behaviors of some special cases:
     *  - Symlinks must be ignored.
     *  - When a directory is added as a shared directory ('addDir(..)') and one or more sub-directories are already shared then
     *    'addDir(..)' is called for the new shared directory and 'rmDir(..)' is called for each old shared sub-directory.
     *  - When a sub-directory of a shared directory is moved as a sub-directory of another shared directory the implementation
     *    can use the events 'DELETED' folloing by 'NEW' or the event 'MOVE' alone, the last one is preferred.
     */
   class DirWatcher
   {
   public:
      virtual ~DirWatcher() {};

      /**
        * Build a new watcher.
        * The implementation is platform dependent.
        *
        * @return 'nullptr' if there is no implementation for the current platform.
        */
      static DirWatcher* getNewWatcher();

      /**
        * Add a directory to watch.
        * Each new added directory is immediately watched. If some modification
        * occurs in the file system bewteen a call of 'addDir' and a call of 'waitEvent' they
        * will be recorded and the next call to 'waitEvent' will be no blocking.
        *
        * @param path An absolute path.
        * @return 'true' if the dir is watchable else 'false'.
        * @exception DirNotFoundException
        */
      virtual bool addDir(const QString& path) = 0;

      /**
        * Remove a watched directory.
        */
      virtual void rmDir(const QString& path) = 0;

      /**
        * Return the number of watched directory.
        */
      virtual int nbWatchedDir() = 0;

      /**
        * Wait a new event from the listened directories or from a given wait condition.
        * There is no timeout, it can wait forever.
        */
      virtual const QList<WatcherEvent> waitEvent(QList<WaitCondition*> ws = QList<WaitCondition*>()) = 0;

      /**
        * Wait a new event from the listened directories or from a given wait condition.
        * @param timeout A timeout in milliseconds. -1 means forever.
        */
      virtual const QList<WatcherEvent> waitEvent(int timeout, QList<WaitCondition*> ws = QList<WaitCondition*>()) = 0;
   };

   /**
     * When a event occurs this struct is returned.
     */
   struct WatcherEvent
   {
      enum Type {
         // A file or a directory is moved into the shared directory structure or a shared directory is moved somewhere else.
         // The file or the directory can be renamed.
         // Some examples on files:
         //  - /home/dlan/shared/a/x.txt -> /home/dlan/shared/a/y.txt
         //  - /home/dlan/shared/a/x.txt -> /home/dlan/shared/a/b/x.txt
         //  - /home/dlan/shared/a/x.txt -> /home/dlan/shared2/a/b/y.txt
         // Some examples on directories:
         //  - /home/dlan/shared/a -> /home/dlan/shared/b
         //  - /home/dlan/shared/a -> /home/dlan/shared/x/a
         //  - /home/dlan/shared/a -> /home/dlan/shared2/a
         // If a file is moved from outside a shared dir you may use 'NEW'
         // If a file is moved from a shared dir to outside you may use 'DELETED'
         MOVE,
         NEW,
         DELETED,
         CONTENT_CHANGED,
         TIMEOUT,
         UNKNOWN
      };

      WatcherEvent();
      WatcherEvent(const WatcherEvent& e);
      WatcherEvent(Type type);
      WatcherEvent(Type type, const QString& path1);
      WatcherEvent(Type type, const QString& path1, const QString& path2);

      /**
        * Default assignment operator does nothing because all members are const.
        */
      WatcherEvent& operator=(const WatcherEvent&) { return *this; }

      QString toStr();

      const Type type;
      const QString path1;
      const QString path2; // Only used when type is 'MOVE'.
   };
}

#endif
