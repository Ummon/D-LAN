/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#include <Common/Global.h>
using namespace Common;

#include <limits>

#include <QDir>
#include <QDirIterator>
#include <QDebug>
#include <QtGlobal>

#ifdef Q_OS_WIN32
   #include <windows.h>
   #include <Shlobj.h>
   #include <Lmcons.h>
#elif defined (Q_OS_LINUX)
   #include <cstdio>
#endif

#include <Constants.h>

/**
  * @class Common::Global
  *
  * Some generic global functions.
  */

Global::UnableToSetTempDirException::UnableToSetTempDirException(const QString& dir) :
   errorMessage(QString("Unable to create the temporary directory %1").arg(dir))
{
}

QString Global::UnableToSetTempDirException::getMessage() const throw()
{
   return this->errorMessage;
}

/**
  * The number of k-combinations (each of size k) from a set S with n elements (size n).
  * @link http://en.wikipedia.org/wiki/Combination
  */
int Global::nCombinations(int n, int k)
{   
   Q_ASSERT(n >= 0);
   Q_ASSERT(k >= 0);

   if (n < 0 || k < 0)
      return 0;

   int c = 1;
   for(int i = 1; i <= k; i++)
      c = c * (n - k + i) / i;
   return c;
}

/**
  * Old implementation, see the other 'formatByteSize(..)' function.
  */
/*QString Global::formatByteSize(qint64 bytes)
{
   const bool IS_BELOW_1024 = bytes < 1024;
   int current = 0;
   int rest = 0;

   if (bytes > 0)
      while (bytes >= 1024)
      {
         rest = ((bytes % 1024) * 1000 / 1024 + 50) / 100;
         bytes /= 1024;
         if (rest >= 10)
         {
            bytes++;
            rest = 0;
         }
         current++;
      }
   else
      bytes = 0;

   return QString::number(bytes).append(IS_BELOW_1024 ? "" : QString(".").append(QString::number(rest))).append(" ").append(BINARY_PREFIXS[current]);
}*/

/**
  * Will return a formated size with the unit prefix and one digit folowing the point.
  * For example :
  * - 1 -> "1 B"
  * - 1024 -> "1.0 KiB"
  * - 1024^2 -> "1.0 MiB"
  * - 1024^3 -> "1.0 GiB"
  * - 1024^4 -> "1.0 TiB"
  * - etc.. to ZiB
  * The speed of this implementation is equal to the old above : ~1 µs per call (mesured with 1 millions calls in release (-O2)).
  */
QString Global::formatByteSize(qint64 bytes, int precision)
{
   Q_ASSERT(precision >= 0);

   for (int i = 0; i < 8; i++)
   {
      qint64 size = 1;
      for (int j = 0; j < i; j++)
         size *= 1024;

      if (bytes < 1024 * size)
         return bytes < 1024 ?
            QString::number(bytes <= 0 ? 0 : bytes).append(" ").append(BINARY_PREFIXS[i]) :
            QString::number((double)bytes / size, 'f', precision).append(" ").append(BINARY_PREFIXS[i]);
   }
   return QString();
}

/**
  * Return the remaining free space for the given path.
  * TODO : Linux
  */
qint64 Global::availableDiskSpace(const QString& path)
{
   Q_ASSERT(!path.isEmpty());

#ifdef Q_OS_WIN32
   ULARGE_INTEGER space;
   wchar_t buffer[path.size()];

   int l = path.toWCharArray(buffer);
   buffer[l] = 0;

   if (!GetDiskFreeSpaceEx(buffer, &space, NULL, NULL))
      return std::numeric_limits<qint64>::max();
   return space.QuadPart;
#endif

   return std::numeric_limits<qint64>::max();
}

/**
  * Rename a file, if 'newFile' already exists, it will be replaced by 'existingFile'.
  * TODO : Linux
  * @remarks Qt doesn't offer any way to replace a file by an other in one operation.
  * @return false if the rename didn't work.
  */
bool Global::rename(const QString& existingFile, const QString& newFile)
{
   Q_ASSERT(!existingFile.isEmpty());
   Q_ASSERT(!newFile.isEmpty());

#ifdef Q_OS_WIN32
   return MoveFileEx((LPCTSTR)existingFile.utf16(), (LPCTSTR)newFile.utf16(), MOVEFILE_REPLACE_EXISTING);
#elif defined(Q_OS_LINUX)
   return false;
#endif
}

/**
  * See QDir::cleanPath(..) documentation.
  * Add a slash at the end.
  */
QString Global::cleanDirPath(const QString& path)
{
   Q_ASSERT(!path.isEmpty());

   return QDir::cleanPath(path).append('/');
}

QString Global::dataFolders[2]; // The two folders (roaming and local), see DataFolderType enum.

/**
  * Returns the absolute path to the roaming data folder.
  * For example under Windows :
  * - type == ROAMING : "C:/Users/john/AppData/Roaming/D-LAN"
  * - type == LOCAL : "C:/Users/john/AppData/Local/D-LAN"
  * Create the folder "D-LAN" if needed.
  * @exception UnableToGetFolder
  */
QString Global::getDataFolder(DataFolderType type, bool create)
{
   if (!Global::dataFolders[type].isEmpty())
   {
      if (create)
         QDir::current().mkpath(Global::dataFolders[type]);
      return Global::dataFolders[type];
   }
   else
   {
#ifdef Q_OS_WIN32
      TCHAR dataPath[MAX_PATH];
      if (!SUCCEEDED(SHGetFolderPath(NULL, type == ROAMING ? CSIDL_APPDATA : CSIDL_LOCAL_APPDATA, NULL, 0, dataPath)))
         throw UnableToGetFolder();

      const QString dataFolderPath = QString::fromUtf16((ushort*)dataPath);
      const QDir dataFolder(dataFolderPath);

      if (create && !dataFolder.exists(APPLICATION_FOLDER_NAME))
         if (!dataFolder.mkdir(APPLICATION_FOLDER_NAME))
            throw UnableToGetFolder();

      return dataFolder.absoluteFilePath(APPLICATION_FOLDER_NAME);
#else
      if (create && !QDir::home().exists(APPLICATION_FOLDER_NAME))
         if (!QDir::home().mkdir(APPLICATION_FOLDER_NAME))
            throw UnableToGetFolder();

      return QDir::home().absoluteFilePath(APPLICATION_FOLDER_NAME);
#endif
   }
}

/**
  * It's possible to override the default data folder for a given type.
  */
void Global::setDataFolder(DataFolderType type, const QString& folder)
{
   if (QDir(folder).exists())
      Global::dataFolders[type] = folder;
}

void Global::setDataFolderToDefault(DataFolderType type)
{
   Global::dataFolders[type].clear();
}

QString Global::getDataSystemFolder(DataFolderType type)
{
// TODO: other platforms.
#ifdef Q_OS_WIN32
   TCHAR dataPathSystem[MAX_PATH];
   if (!SUCCEEDED(SHGetFolderPath(NULL, CSIDL_SYSTEMX86, NULL, 0, dataPathSystem)))
      return QString();

   const QString dataFolderPath = QString::fromUtf16((ushort*)dataPathSystem);

   return dataFolderPath + "/config/systemprofile/AppData" + (type == ROAMING ? "/Roaming/" : "/local/") + APPLICATION_FOLDER_NAME;
#else
   return QString();
#endif
}

QString Global::getCurrenUserName()
{
#ifdef Q_OS_WIN32
   TCHAR userName[UNLEN + 1]; // UNLEN is from Lmcons.h
   DWORD userNameSize = sizeof(userName);
   GetUserName(userName, &userNameSize);
   return QString::fromUtf16((ushort*)userName);
#else // TODO
   return "Bob";
#endif
}

QString Global::getCurrenMachineName()
{
#ifdef Q_OS_WIN32
   TCHAR machineName[MAX_COMPUTERNAME_LENGTH + 1];
   DWORD machineNameSize = sizeof(machineName);
   GetComputerName(machineName, &machineNameSize);
   return QString::fromUtf16((ushort*)machineName);
#else // TODO
   return "CPU";
#endif
}

/**
  * Create a file containing its name. Parents directories are created if needed.
  * For testing purpose.
  * @return true if the file has been created successfuly or false if an error has occured.
  */
bool Global::createFile(const QString& path)
{
   Q_ASSERT(!path.isEmpty());

   QFileInfo fileInfo(path);
   if (!QDir::current().mkpath(fileInfo.path()))
      return false;

   // If fileName is empty, the job is just to
   // create a new folder, then we can exit now.
   if (fileInfo.fileName().isEmpty())
      return true;

   QFile file(path);
   if (!file.open(QIODevice::WriteOnly))
      return false;

   QTextStream stream(&file);
   stream << fileInfo.fileName();

   return true;
}

/**
  * For testing purpose.
  */
bool Global::recursiveDeleteDirectoryContent(const QString& dir)
{
   Q_ASSERT(!dir.isEmpty());

   bool success = true;

   for (QDirIterator i(dir, QDir::Files, QDirIterator::Subdirectories); i.hasNext();)
      if (!QFile(i.next()).remove())
         success = false;

   for (QDirIterator i(dir, QDir::AllDirs | QDir::NoDotAndDotDot, QDirIterator::Subdirectories); i.hasNext();)
      if (!QDir::current().rmpath(i.next()))
         success = false;

   return success;
}

/**
  * For testing purpose.
  */
bool Global::recursiveDeleteDirectory(const QString& dir)
{
   Q_ASSERT(!dir.isEmpty());

   bool success = Global::recursiveDeleteDirectoryContent(dir);

   if (QDir::current().exists(dir) && !QDir(dir).rmdir("."))
      success = false;

   return success;
}

/**
  * Create a directory into the temp directory and set as the current one.
  * For testing purpose.
  * @exception UnableToSetTempDirException
  */
QString Global::setCurrentDirToTemp(const QString& dirname)
{
   Q_ASSERT(!dirname.isEmpty());

   const QString TEMP_DIRNAME("D-LAN " + dirname);
   QDir::setCurrent(QDir::tempPath());
   if (!QDir::current().exists(TEMP_DIRNAME))
      if (!QDir::current().mkdir(TEMP_DIRNAME))
         throw UnableToSetTempDirException(QDir(TEMP_DIRNAME).absolutePath());

   QDir dir;
   dir.cd(TEMP_DIRNAME);
   QDir::setCurrent(dir.absolutePath());
   return dir.absolutePath();
}
