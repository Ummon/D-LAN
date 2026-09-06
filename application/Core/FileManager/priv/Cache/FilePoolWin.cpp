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

#include <priv/Cache/FilePool.h>
#include <priv/Log.h>
#include <windows.h>
#include <io.h>
#include <fcntl.h>
using namespace FM;

bool FilePool::canReuseReleasedFiles()
{
   // openFile() omits FILE_SHARE_DELETE, preventing replacement while the handle is cached.
   return true;
}

namespace
{
   DWORD toCreateFileDesiredAccess(QIODevice::OpenMode mode)
   {
      if (mode.testFlag(QIODevice::ReadWrite))
         return GENERIC_READ | GENERIC_WRITE;
      else
         return GENERIC_READ;
   }

   DWORD toCreateFileCreationDisposition(QIODevice::OpenMode mode)
   {
      if (mode.testFlag(QIODevice::ReadWrite))
         return OPEN_ALWAYS;
      else
         return OPEN_EXISTING;
   }

   int toCreateFileOpenHandleFlag(QIODevice::OpenMode mode)
   {
      if (mode.testFlag(QIODevice::ReadWrite))
         return _O_RDWR;
      else
         return _O_RDONLY;
   }
}

QFile* FilePool::openFile(const QString& path, QIODevice::OpenMode mode, bool* fileCreated)
{
   // We use the 'CreateFileW' function to control the sharing mode of the file.
   HANDLE h =
      CreateFileW(
         reinterpret_cast<LPCWSTR>(path.utf16()),
         toCreateFileDesiredAccess(mode),
         FILE_SHARE_READ | FILE_SHARE_WRITE, // A file being downloaded (opened in write mode) must also be readable by us: to upload it and to check the integrity of a resumed chunk.
         nullptr,
         toCreateFileCreationDisposition(mode),
         FILE_ATTRIBUTE_NORMAL,
         nullptr
      );
   const DWORD lastError = GetLastError();

   if (h == INVALID_HANDLE_VALUE)
   {
      L_DEBU(
         QString("FilePool::open(%1, %2): invalid handle, error: %3")
            .arg(path)
            .arg(mode.toInt())
            .arg(lastError)
      );
      return nullptr;
   }

   int fd = _open_osfhandle(reinterpret_cast<intptr_t>(h), toCreateFileOpenHandleFlag(mode));
   if (fd == -1)
   {
      CloseHandle(h);
      return nullptr;
   }

   QFile* file = new QFile();

   if (fileCreated && mode.testFlag(QIODevice::WriteOnly))
      *fileCreated = lastError != ERROR_ALREADY_EXISTS;

   if (!file->open(fd, mode, QFileDevice::AutoCloseHandle))
   {
      if (fileCreated)
         *fileCreated = false;
      delete file;
      _close(fd);
      return nullptr;
   }

   return file;
}
