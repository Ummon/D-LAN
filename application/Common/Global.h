#ifndef COMMON_COMMON_H
#define COMMON_COMMON_H

#include <QtGlobal>

namespace Common
{
   class Global
   {
   public:
      static int nCombinations(int n, int k);
      static QString formatByteSize(qint64 bytes);
      static qint64 availableDiskSpace(const QString& path);
      static bool createApplicationFolder();
      static void createFile(const QString& path);
      static bool recursiveDeleteDirectoryContent(const QString& dir);
      static bool recursiveDeleteDirectory(const QString& dir);
   };
}

#endif
