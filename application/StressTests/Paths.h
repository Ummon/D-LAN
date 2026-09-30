#pragma once

#include <QString>

namespace StressTests
{
   /**
     * Clean path with '/' as separators and without trailing '/'. Lowercase if the file system is case insensitive.
     * Only for comparisons.
     */
   QString normalizePath(const QString& path);

   bool isSamePath(const QString& path1, const QString& path2);

   /**
     * True if 'path' is 'directory' or is inside 'directory'.
     */
   bool isSameOrInside(const QString& path, const QString& directory);
}
