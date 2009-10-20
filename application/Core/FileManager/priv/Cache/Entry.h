#ifndef FILEMANAGER_ENTRY_H
#define FILEMANAGER_ENTRY_H

#include <QString>

namespace FileManager
{
   class Directory;
   class Entry
   {
   public:
      Entry(const QString& name);
      Entry(const QString& name, qint64 size);
      virtual ~Entry() {};

      /**
        * Return the relative path from the root directory.
        * It's the directory in which the entry is.
        * For example : "animals/fish"
        */
      virtual QString getPath() = 0;

      /**
        * Return the full absolute path to the entry.
        */
      virtual QString getFullPath() = 0;
      virtual Directory* getRoot() = 0;

      virtual QString getName();
      virtual qint64 getSize();

   protected:
      QString name;
      qint64 size;
   };
}
#endif
