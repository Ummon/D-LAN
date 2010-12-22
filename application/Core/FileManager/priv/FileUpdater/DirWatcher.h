/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
   public:
      virtual ~DirWatcher() {};

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
        * @return 'true' is the dir is watchable else 'false'.
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
         // A file moved into the shared directory. The file can be renamed.
         // Some examples :
         // - shared/a/x.txt -> shared/a/y.txt
         // - shared/a/x.txt -> shared/a/b/x.txt
         // - shared/a/x.txt -> shared/a/b/y.txt
         // If a file is moved from outside to a shared dir you may use NEW
         // If a file is moved from a shared dir to outside you may use DELETED
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
      const QString path2; // Only used when type == MOVE.
   };
}

#endif
