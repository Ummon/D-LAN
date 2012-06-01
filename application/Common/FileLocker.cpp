/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <Common/FileLocker.h>
using namespace Common;

#ifdef Q_OS_WIN32
   #include <io.h>
   #include <WinIoCtl.h>
#endif

/**
  * @class FileLocker
  *
  * The purpose of this class is to be able to easy lock a part of a file in a portable way.
  * The file is locked in the constructor and automatically unlocked in the destructor.
  *
  * @remarks Only the Windows implementation currently exists.
  */

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

/**
  * After a 'FileLocker' is built this method should be called to test if the file has been properly locked.
  */
bool FileLocker::isLocked() const
{
   return this->lockAcquired;
}
