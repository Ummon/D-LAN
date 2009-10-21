#ifndef FILEMANAGER_CACHE_H
#define FILEMANAGER_CACHE_H

#include <QObject>
#include <QList>
#include <QStringList>

#include <priv/Cache/SharedDirectory.h>

namespace FileManager
{
   class SharedDirectory;
   class Entry;
   class FileUpdater;
   class FileManager;

   class Cache : public QObject
   {
      Q_OBJECT
   public:
      Cache(FileManager* fileManager, FileUpdater* fileUpdater);

      QStringList getSharedDirs(SharedDirectory::Rights rights);

      /**
        * @exception DirsNotFoundException
        */
      void setSharedDirs(const QStringList& dirs, SharedDirectory::Rights rights);

      void onEntryAdded(Entry* entry);
      void onEntryRemoved(Entry* entry);

   signals:
      void entryAdded(Entry* entry);
      void entryRemoved(Entry* entry);

   private:
      QList<SharedDirectory*> sharedDirs;

      FileManager* fileManager;
      FileUpdater* fileUpdater;
   };
}
#endif
