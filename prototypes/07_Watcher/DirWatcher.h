#ifndef DIRWATCHER_H
#define DIRWATCHER_H

#include <QtCore/QList>
#include <QtCore/QString>

struct WatcherEvent;

/**
  * Event types :
  *  - Rename dir
  *  - Rename file
  *  - New file
  *  - Delete file
  *  - The size of a file changed
  */
class DirWatcher
{
protected:
   DirWatcher();
public:
   virtual ~DirWatcher();
   
   /**
     * Build a new watcher.
     */
   static DirWatcher* getNewWatcher();

   /**
     * A a directory to watch.
     */
   virtual void addDir(const QString& path) = 0;

   /**
     * Remove a watched directory.
     */
   virtual void rmDir(const QString& path) = 0;
   
   /**
     * Wait a new event from the listened directories.
     * @param timeout A timeout in milliseconds.
     */
   virtual WatcherEvent waitEvent(int timeout) = 0;
};

struct WatcherEvent
{
   enum Type {
      RENAME_DIR,
      RENAME_FILE,
      NEW_FILE,
      DELETE_FILE,
      SIZE_FILE_CHANGED      
   };
   
   WatcherEvent(Type type, const QString& path1);
   WatcherEvent(Type type, const QString& path1, const QString& path2);
  
   Type type;
   const QString path1;
   const QString path2;
};

#endif // DIRWATCHER_H
