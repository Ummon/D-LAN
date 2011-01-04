#ifndef FILEMANAGER_GLOBAL_H
#define FILEMANAGER_GLOBAL_H

#include <QString>
namespace FM
{
   class Global
   {
   public:
      static const QString& getUnfinishedSuffix();
      static bool isFileUnfinished(const QString& filename);
      static QString removeUnfinishedSuffix(const QString& filename);
   };
}

#endif
