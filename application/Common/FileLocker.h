#ifndef COMMON_FILELOCKER_H
#define COMMON_FILELOCKER_H

#include <QtGlobal>

#ifdef Q_OS_WIN32
   #include <windows.h>
#endif

#include <QString>
#include <QFile>

namespace Common
{
   class FileLocker
   {
   public:
      enum LockType { READ, WRITE };

      FileLocker(const QFile& file, qint64 nbBytesToLock, LockType type);
      ~FileLocker();

      bool isLocked() const;

   private:
      bool lockAcquired;
      const qint64 nbBytesLocked;

#ifdef Q_OS_WIN32
      HANDLE fileHandle;
      OVERLAPPED overlapped;
#endif
   };
}

#endif
