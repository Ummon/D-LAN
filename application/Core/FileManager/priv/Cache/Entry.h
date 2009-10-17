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
      virtual ~Entry() {};

      /**
        * Return the full absolute path to the entry.
        */
      virtual QString getPath() = 0;
      virtual Directory* getRoot() = 0;

      virtual QString getName();
      virtual qint64 getSize();

   protected:
      QString name;
      qint64 size;
   };
}
#endif
