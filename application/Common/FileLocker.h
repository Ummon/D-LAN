#ifndef COMMON_FILELOCKER_H
#define COMMON_FILELOCKER_H

#include <QString>

namespace Common
{
   class FileLocker
   {
   public:
      enum LockType { READ, WRITE };

      FileLocker(const QString& filePath, LockType type);
      ~FileLocker();

      bool isLocked() const;
   };
}

#endif
