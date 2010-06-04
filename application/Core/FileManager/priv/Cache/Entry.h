#ifndef FILEMANAGER_ENTRY_H
#define FILEMANAGER_ENTRY_H

#include <QString>

#include <Protos/common.pb.h>

namespace FM
{
   class Directory;
   class Cache;

   class Entry
   {
   public:
      Entry(Cache* cache, const QString& name, qint64 size = 0);
      virtual ~Entry();

   protected:
      void populateEntry(Protos::Common::Entry* entry) const;

   public:

      Cache* getCache();

      /**
        * Return the relative path from the root directory.
        * It's the directory in which the entry is.
        * For example : "/animals/fish/"
        * An entry in a root will have a path like "/".
        * A root (SharedDirectory) will have an empty path ("").
        */
      virtual QString getPath() const = 0;

      /**
        * Return the full absolute path to the entry.
        */
      virtual QString getFullPath() const = 0;
      virtual Directory* getRoot() const = 0;

      virtual const QString& getName() const;

      /**
        * When a file or a directory is renamed.
        */
      virtual void changeName(const QString& newName);

      virtual qint64 getSize() const;

   protected:
      Cache* cache;

      QString name;
      qint64 size;
   };
}
#endif
