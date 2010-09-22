#ifndef COMMON_COMMON_H
#define COMMON_COMMON_H

#include <QtGlobal>

namespace Common
{
   class Global
   {
   public:
      static qint64 availableDiskSpace(const QString& path);
      static bool createApplicationFolder();
      static void createFile(const QString& path);
      static void recursiveDeleteDirectory(const QString& dir);
   };
}

#endif
