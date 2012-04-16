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
  
#include <Common/Global.h>
using namespace Common;

#include <limits>

#include <QDir>
#include <QDirIterator>
#include <QStringBuilder>
#include <QtGlobal>
#include <QRegExp>
#include <QHostAddress>
#include <QNetworkInterface>

#ifdef Q_OS_WIN32
   #include <windows.h>
   #include <Shlobj.h>
   #include <Lmcons.h>
#elif defined (Q_OS_LINUX)
   #include <cstdio>
   #include <sys/statvfs.h>
   #include <sys/utsname.h>
   #include <unistd.h>
#endif

#include <Constants.h>
#include <Version.h>

/**
  * @class Common::Global
  *
  * Some generic global functions.
  */

Global::UnableToSetTempDirException::UnableToSetTempDirException(const QString& dir) :
   errorMessage(QString("Unable to create the temporary directory %1").arg(dir))
{
}

QString Global::getVersion()
{
   return QString(VERSION);
}

QString Global::getVersionTag()
{
   return QString(VERSION_TAG);
}

QString Global::getSystemVersion()
{
#if defined(Q_OS_WIN32)
   // Reference: http://msdn.microsoft.com/en-us/library/windows/desktop/ms724429(v=vs.85).aspx
   OSVERSIONINFOEX versionInfo;
   memset(&versionInfo, 0, sizeof(versionInfo));
   versionInfo.dwOSVersionInfoSize = sizeof(versionInfo);
   GetVersionEx((OSVERSIONINFO*)&versionInfo);

   if (versionInfo.dwMajorVersion == 6 && versionInfo.dwMinorVersion == 0)
   {
      if (versionInfo.wProductType == VER_NT_WORKSTATION)
         return QString("Windows Vista");
      else
         return QString("Windows Server 2008");
   }
   else if (versionInfo.dwMajorVersion == 6 && versionInfo.dwMinorVersion == 1)
   {
      if (versionInfo.wProductType == VER_NT_WORKSTATION)
         return QString("Windows 7");
      else
         return QString("Windows Server 2008 R2");
   }
   else if (versionInfo.dwMajorVersion == 5 && versionInfo.dwMinorVersion == 2)
   {
      return QString("Windows Server 2003");
   }
   else if (versionInfo.dwMajorVersion == 5 && versionInfo.dwMinorVersion == 1)
   {
      return QString("Windows XP");
   }
   else if (versionInfo.dwMajorVersion == 5 && versionInfo.dwMinorVersion == 0)
   {
      return QString("Windows 2000");
   }
   return QString("Windows");
#elif defined(Q_OS_LINUX)
   struct utsname name;
   if (uname(&name) == 0)
      return QString::fromUtf8(name.sysname) % " " % QString::fromUtf8(name.release);
   else
      return "Linux";
#elif defined(Q_OS_DARWIN)
   return "Mac OS X";
#else
   return QString();
#endif
}

/**
  * @return Version + version tag + system version.
  */
QString Global::getVersionFull()
{
   const QString versionTag = Global::getVersionTag();
   const QString systemVersion = Global::getSystemVersion();
   return
      Global::getVersion() %
      (versionTag.isEmpty() ? QString() : " " % Global::getVersionTag()) %
      (systemVersion.isEmpty() ? QString() : " - " % Global::getSystemVersion());
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

   return QString::number(bytes).append(IS_BELOW_1024 ? "" : QString(".").append(QString::number(rest))).append(" ").append(Constants::BINARY_PREFIXS[current]);
}*/

/**
  * Will return a formated size with the unit prefix and one digit folowing the point.
  * For example:
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
            QString::number(bytes <= 0 ? 0 : bytes).append(" ").append(Constants::BINARY_PREFIXS[i]) :
            QString::number((double)bytes / size, 'f', precision).append(" ").append(Constants::BINARY_PREFIXS[i]);
   }
   return QString();
}

/**
  * Format the given time in a years / months / weeks / days / hours / minutes / seconds format.
  * Examples:
  *  - "34s"
  *  - "12m" (0 second)
  *  - "12m 3s"
  *  - "64h 23m"
  *  - "1y" (0 month)
  */
QString Global::formatTime(quint64 seconds)
{
   QString output;

   int ymwdhms[7];
   char units[7] = {'y', 'M', 'w', 'd', 'h', 'm', 's'};

   /* years   */ ymwdhms[0] = seconds / (60 * 60 * 24 * 7 * 4 * 12);
   /* months  */ ymwdhms[1] = seconds / (60 * 60 * 24 * 7 * 4) - (12 * ymwdhms[0]);
   /* weeks   */ ymwdhms[2] = seconds / (60 * 60 * 24 * 7) - (4 * 12 * ymwdhms[0] + 4 * ymwdhms[1]);
   /* days    */ ymwdhms[3] = seconds / (60 * 60 * 24) - (7 * 4 * 12 * ymwdhms[0] + 7 * 4 * ymwdhms[1] + 7 * ymwdhms[2]);
   /* hours   */ ymwdhms[4] = seconds / (60 * 60) - (24 * 7 * 4 * 12 * ymwdhms[0] + 24 * 7 * 4 * ymwdhms[1] + 24 * 7 * ymwdhms[2] + 24 * ymwdhms[3]);
   /* minutes */ ymwdhms[5] = seconds / (60) - (60 * 24 * 7 * 4 * 12 * ymwdhms[0] + 60 * 24 * 7 * 4 * ymwdhms[1] + 60 * 24 * 7 * ymwdhms[2] + 60 * 24 * ymwdhms[3] + 60 * ymwdhms[4]);
   /* seconds */ ymwdhms[6] = seconds  -  (60 * 60 * 24 * 7 * 4 * 12 * ymwdhms[0] + 60 * 60 * 24 * 7 * 4 * ymwdhms[1] + 60 * 60 * 24 * 7 * ymwdhms[2] + 60 * 60 * 24 * ymwdhms[3] + 60 * 60 * ymwdhms[4] + 60 * ymwdhms[5]);

   bool exit = false;
   for (int i = 0; i < 7; i++)
   {
      exit = !output.isEmpty();
      if (ymwdhms[i] != 0)
      {
         if (!output.isEmpty())
            output.append(' ');
         output.append(QString::number(ymwdhms[i])).append(QChar(units[i]));
      }
      if (exit)
         break;
   }

   return output;
}

QString Global::formatIP(const QHostAddress& address, quint16 port)
{
   QString formatedIP;
   if (address.protocol() == QAbstractSocket::IPv4Protocol)
      formatedIP.append(address.toString());
   else
      formatedIP.append("[").append(address.toString()).append("]");
   formatedIP.append(":").append(QString::number(port));
   return formatedIP;
}

/**
  * Return the remaining free space for the given path.
  */
qint64 Global::availableDiskSpace(const QString& path)
{
   Q_ASSERT(!path.isEmpty());

#if defined(Q_OS_WIN32)
   ULARGE_INTEGER space;
   wchar_t buffer[path.size()];

   int l = path.toWCharArray(buffer);
   buffer[l] = 0;

   if (!GetDiskFreeSpaceEx(buffer, &space, NULL, NULL))
      return std::numeric_limits<qint64>::max();
   return space.QuadPart;
#elif defined(Q_OS_LINUX)
   struct statvfs info;
   if (statvfs(path.toUtf8().constData(), &info) == 0)
      return static_cast<qint64>(info.f_bsize) * info.f_bavail;
#endif

   return std::numeric_limits<qint64>::max();
}

/**
  * Rename a file, if 'newFile' already exists, it will be replaced by 'existingFile'.
  * @remarks Qt doesn't offer any way to replace a file by an other in one operation (atomic).
  * @return false if the rename didn't work.
  */
bool Global::rename(const QString& existingFile, const QString& newFile)
{
   Q_ASSERT(!existingFile.isEmpty());
   Q_ASSERT(!newFile.isEmpty());

#ifdef Q_OS_WIN32
   return MoveFileEx((LPCTSTR)existingFile.utf16(), (LPCTSTR)newFile.utf16(), MOVEFILE_REPLACE_EXISTING);
#else
   return std::rename(existingFile.toUtf8().constData(), newFile.toUtf8().constData()) == 0;
#endif
}

bool Global::isLocal(const QHostAddress& address)
{
   return address == QHostAddress::LocalHost || address == QHostAddress::LocalHostIPv6 || QNetworkInterface::allAddresses().contains(address);
}

/**
  * See QDir::cleanPath(..) documentation.
  * Add a slash at the end.
  */
QString Global::cleanDirPath(const QString& path)
{
   Q_ASSERT(!path.isEmpty());

   QString cleanedPath = QDir::cleanPath(path);
   if (!cleanedPath.isEmpty() && cleanedPath[cleanedPath.size()-1] != '/')
      cleanedPath.append('/');

   return cleanedPath;
}

/**
  * Special case for Windows: "C:/" -> "C:"
  */
QString Global::dirName(const QString& path)
{
   Q_ASSERT(!path.isEmpty());

#ifdef Q_OS_WIN32
   if (path.size() == 3 && path[1] == ':')
      return path.left(2);
#endif
   return QDir(path).dirName();
}

QString Global::toLowerAndRemoveAccents(const QString& str)
{
   QString strLower = str.toLower();

   for (int i = 0; i < strLower.size(); i++)
      switch (strLower[i].unicode())
      {
      case 0x00E0: // à .
      case 0x00E1: // á.
      case 0x00E2: // â.
      case 0x00E3: // ã.
      case 0x00E4: // ä.
      case 0x00E5: // å.
         strLower[i] = 'a';
         break;
      case 0x00E7: // ç.
         strLower[i] = 'c';
         break;
      case 0x00E8: // è.
      case 0x00E9: // é.
      case 0x00EA: // ê.
      case 0x00EB: // ë.
         strLower[i] = 'e';
         break;
      case 0x00EC: // ì.
      case 0x00ED: // í.
      case 0x00EE: // î.
      case 0x00EF: // ï.
         strLower[i] = 'i';
         break;
      case 0x00F1: // ñ.
         strLower[i] = 'n';
         break;
      case 0x00F2: // ò.
      case 0x00F3: // ó.
      case 0x00F4: // ô.
      case 0x00F5: // õ.
      case 0x00F6: // ö.
         strLower[i] = 'o';
         break;
      case 0x00F9: // ù.
      case 0x00FA: // ú.
      case 0x00FB: // û.
      case 0x00FC: // ü.
         strLower[i] = 'u';
         break;
      }

   return strLower;
}

/**
  * Take raw terms in a string and split, trim and filter to
  * return a list of keyword.
  * Some character or word can be removed.
  * Maybe a class 'WordSplitter' should be created.
  * @example " The little  DUCK " => ["the", "little", "duck"].
  */
QStringList Global::splitInWords(const QString& words)
{
   const static QRegExp regExp("(\\W+|_)");
   return Global::toLowerAndRemoveAccents(words).split(regExp, QString::SkipEmptyParts);
}

/**
  * Compare two std::string without case sensitive.
  * @return 0 if equal, 1 if s1 > s2, -1 if s1 < s2.
  */
int Global::strcmpi(const std::string& s1, const std::string& s2)
{
   for (unsigned int i = 0; i < s1.length() && i < s2.length(); i++)
   {
      const int c1 = tolower(s1[i]);
      const int c2 = tolower(s2[i]);
      if (c1 > c2) return 1;
      else if (c1 < c2) return -1;
   }
   if (s1.length() > s2.length())
      return 1;
   else if (s1.length() < s2.length())
      return -1;
   return 0;
}

quint32 Global::hashStringToInt(const QString& str)
{
   QByteArray data = str.toLocal8Bit();
   if (data.size() <= 1)
      return qChecksum(data.constData(), data.size());

   const int s = data.size();
   const quint32 part1 = qChecksum(data.constData(), s / 2);
   const quint32 part2 = qChecksum(data.constData() + s / 2, s / 2 + (s % 2 == 0 ? 0 : 1));
   return part1 | part2 << 16;
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
         throw UnableToGetFolder(QString("Unable to get the %1: SHGetFolderPath failed").arg(type == ROAMING ? "roaming user directory path" : "local user directory path"));

      const QString dataFolderPath = QString::fromUtf16((ushort*)dataPath);
      const QDir dataFolder(dataFolderPath);

      if (create && !dataFolder.exists(Constants::APPLICATION_FOLDER_NAME))
         if (!dataFolder.mkdir(Constants::APPLICATION_FOLDER_NAME))
            throw UnableToGetFolder(QString("Unable to create the directory %1 in %2").arg(Constants::APPLICATION_FOLDER_NAME).arg(dataFolder.absolutePath()));

      return dataFolder.absoluteFilePath(Constants::APPLICATION_FOLDER_NAME);
#else
      if (create && !QDir::home().exists(Constants::APPLICATION_FOLDER_NAME))
         if (!QDir::home().mkdir(Constants::APPLICATION_FOLDER_NAME))
             throw UnableToGetFolder(QString("Unable to create the directory %1 in %2").arg(Constants::APPLICATION_FOLDER_NAME).arg(QDir::home().absolutePath()));

      return QDir::home().absoluteFilePath(Constants::APPLICATION_FOLDER_NAME);
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

/**
  * Return where the local services put their data.
  * It's used to retrieve the data folder of D-LAN.Core when run as a service.
  */
QString Global::getDataServiceFolder(DataFolderType type)
{
// TODO: other platforms.
#ifdef Q_OS_WIN32
   OSVERSIONINFO versionInfo;
   memset(&versionInfo, 0, sizeof(versionInfo));
   versionInfo.dwOSVersionInfoSize = sizeof(versionInfo);
   GetVersionEx(&versionInfo);

   // Vista & Windows 7
   if (versionInfo.dwMajorVersion >= 6)
      return Global::getDataSystemFolder(type);
   else
   {
      // For Windows XP, the service data folder is located in C:\Documents and Settings\LocalService.

      TCHAR windowsPathTCHAR[MAX_PATH];
      if (!SUCCEEDED(SHGetFolderPath(NULL, CSIDL_WINDOWS , NULL, 0, windowsPathTCHAR)))
         return QString();
      const QString windowsPath = QString::fromUtf16((ushort*)windowsPathTCHAR);
      QStringList windowsPathSplit = windowsPath.split('\\');
      if (windowsPathSplit.isEmpty())
         return QString();

      return windowsPathSplit.first().replace('\\', '/') + "/Documents and Settings/LocalService" + (type == ROAMING ? "/Application Data/" : "/Local Settings/Application Data/") + Constants::APPLICATION_FOLDER_NAME;
   }
#else
   return Global::getDataSystemFolder(type);
#endif
}

/**
  * For example on Windows 7 : "C:\Windows\SysWOW64\config\systemprofile\AppData".
  */
QString Global::getDataSystemFolder(DataFolderType type)
{
// TODO: other platforms.
#ifdef Q_OS_WIN32
   TCHAR dataPathSystem[MAX_PATH];
   // SHGetKnownFolderPath should be use for vista a superior but it doesn't exist in mingw.
   if (!SUCCEEDED(SHGetFolderPath(NULL, CSIDL_SYSTEMX86, NULL, 0, dataPathSystem)))
      return QString();
   const QString dataFolderPath = QString::fromUtf16((ushort*)dataPathSystem).replace('\\', '/');

   OSVERSIONINFO versionInfo;
   memset(&versionInfo, 0, sizeof(versionInfo));
   versionInfo.dwOSVersionInfoSize = sizeof(versionInfo);
   GetVersionEx(&versionInfo);

   // Vista & Windows 7
   if (versionInfo.dwMajorVersion >= 6)
      return dataFolderPath + "/config/systemprofile/AppData" + (type == ROAMING ? "/Roaming/" : "/local/") + Constants::APPLICATION_FOLDER_NAME;
   else
      return dataFolderPath + "/config/systemprofile" + (type == ROAMING ? "/Application Data/" : "/Local Settings/Application Data/") + Constants::APPLICATION_FOLDER_NAME;
#else
   return QString();
#endif
}

QString Global::getCurrenUserName()
{
#if defined(Q_OS_WIN32)
   TCHAR userName[UNLEN + 1]; // UNLEN is from Lmcons.h
   DWORD userNameSize = sizeof(userName);
   GetUserName(userName, &userNameSize);
   return QString::fromUtf16((ushort*)userName);
#elif defined(Q_OS_LINUX)
   char* login = getlogin();
   if (login)
      return QString::fromUtf8(login);
   else
      return QString();
#else
   return "Bob";
#endif
}

QString Global::getCurrenMachineName()
{
#if defined(Q_OS_WIN32)
   TCHAR machineName[MAX_COMPUTERNAME_LENGTH + 1];
   DWORD machineNameSize = sizeof(machineName);
   GetComputerName(machineName, &machineNameSize);
   return QString::fromUtf16((ushort*)machineName);
#elif defined(Q_OS_LINUX)
   char machineName[256];
   size_t machineNameSize = sizeof(machineName);
   gethostname(machineName, machineNameSize);
   return QString::fromUtf8(machineName);
#else
   return "Bob";
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
   const QString tempDir = dir.absolutePath();
   QDir::setCurrent(tempDir);
   return tempDir;
}
