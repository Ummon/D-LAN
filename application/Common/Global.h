#ifndef COMMON_COMMON_H
#define COMMON_COMMON_H

#include <QtGlobal>

namespace Common
{
   class Global
   {
   public:
      static bool createApplicationFolder();
      static qint64 availableDiskSpace(const QString& path);
   };
}

#endif
