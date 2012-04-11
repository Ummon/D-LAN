#include <Common/FileLocker.h>
using namespace Common;

#ifdef Q_OS_WIN32
   #include <io.h>
   #include <WinIoCtl.h>
#endif

FileLocker::FileLocker(const QFile& file, qint64 nbBytesToLock, LockType type) :
   nbBytesLocked(nbBytesToLock)
 #ifdef Q_OS_WIN32
   ,fileHandle((HANDLE)_get_osfhandle(file.handle()))
 #endif
{
#ifdef Q_OS_WIN32
   this->overlapped.hEvent = 0;

   this->overlapped.Offset = static_cast<DWORD>(file.pos() & 0x00000000FFFFFFFFLL);
   this->overlapped.OffsetHigh = static_cast<DWORD>(file.pos() >> 32 & 0x00000000FFFFFFFFLL);

   this->lockAcquired = LockFileEx(
      this->fileHandle,
      LOCKFILE_FAIL_IMMEDIATELY | (type == WRITE ? LOCKFILE_EXCLUSIVE_LOCK : 0),
      0,
      static_cast<DWORD>(this->nbBytesLocked & 0x00000000FFFFFFFFLL),
      static_cast<DWORD>(this->nbBytesLocked >> 32 & 0x00000000FFFFFFFFLL),
      &this->overlapped
   );
#else
   this->lockAcquired = true;
#endif
}

FileLocker::~FileLocker()
{
#ifdef Q_OS_WIN32
   UnlockFileEx(
      this->fileHandle,
      0,
      static_cast<DWORD>(this->nbBytesLocked & 0x00000000FFFFFFFFLL),
      static_cast<DWORD>(this->nbBytesLocked >> 32 & 0x00000000FFFFFFFFLL),
      &this->overlapped
   );
#endif
}

bool FileLocker::isLocked() const
{
   return this->lockAcquired;
}
