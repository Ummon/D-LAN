#ifndef FILEMANAGER_ENTRY_H
#define FILEMANAGER_ENTRY_H

#include <QString>

#include <Common/Deletable.h>

namespace FM
{
   class Directory;
   class Cache;

   class Entry : public Common::Deletable
   {
   public:
      Entry(Cache* cache, const QString& name, qint64 size = 0);
      virtual ~Entry();

      Cache* getCache();

      /**
        * Return the relative path from the root directory.
        * It's the directory in which the entry is.
        * For example : "animals/fish"
        */
      virtual QString getPath() const = 0;

      /**
        * Return the full absolute path to the entry.
        */
      virtual QString getFullPath() const = 0;
      virtual Directory* getRoot() const = 0;

      virtual const QString& getName() const;
      virtual qint64 getSize() const;

   protected:
      Cache* cache;

      QString name;
      qint64 size;
   };
}
#endif
