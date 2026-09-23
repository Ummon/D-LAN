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
#include <QCoreApplication>
#include <QDirIterator>
#include <QStringBuilder>
#include <QByteArray>
#include <QFileInfo>
#include <QSet>
#include <QStandardPaths>
#include <QtGlobal>
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
#elif defined(Q_OS_DARWIN)
   #include <SystemConfiguration/SystemConfiguration.h>
   #include <cerrno>
   #include <pwd.h>
   #include <sys/mount.h>
   #include <unistd.h>
#endif

#include <Constants.h>
#include <Version.h>
#include <StringUtils.h>

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
   static const QString systemVersion = QSysInfo::prettyProductName();
   return systemVersion;
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
      (versionTag.isEmpty() ? QString() : " " % versionTag) %
      (systemVersion.isEmpty() ? QString() : " - " % systemVersion);
}

QDateTime Global::getBuildTime()
{
   return QDateTime::fromString(BUILD_TIME, "yyyy-MM-dd_HH-mm");
}

QString Global::getCompilerName()
{
#if defined(__clang__)
   return "Clang";
#elif defined(__GNUC__)
   return "GCC";
#else
   return "";
#endif
}

QString Global::getCompilerVersion()
{
#if defined(__clang__)
   return
      QString::number(__clang_major__)
         .append(".")
         .append(QString::number(__clang_minor__))
         .append(".").append(QString::number(__clang_patchlevel__));
#elif defined(__GNUC__)
   return
      QString::number(__GNUC__)
         .append(".")
         .append(QString::number(__GNUC_MINOR__))
         .append(".").append(QString::number(__GNUC_PATCHLEVEL__));
#else
   return "";
#endif
}

/**
  * The number of k-combinations (each of size k) from a set S with n elements (size n).
  * http://en.wikipedia.org/wiki/Combination
  */
int Global::nCombinations(int n, int k)
{
   Q_ASSERT(n >= 0);
   Q_ASSERT(k >= 0);

   if (n < 0 || k < 0 || k > n)
      return 0;

   // C(n, k) == C(n, n - k), taking the smallest one needs fewer steps and grows less.
   if (k > n - k)
      k = n - k;

   qint64 c = 1;
   for (int i = 1; i <= k; i++)
   {
      // 'c' is at most INT_MAX here and (n - k + i) at most n, thus the product can't overflow a 'qint64'.
      c = c * (n - k + i) / i;

      // The result is saturated rather than silently wrapped around.
      if (c > std::numeric_limits<int>::max())
         return std::numeric_limits<int>::max();
   }
   return static_cast<int>(c);
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

   return QString::number(bytes).append(IS_BELOW_1024 ? "" : QString(".").append(QString::number(rest))).append(" ").append(Constants::BINARY_PREFIXES[current]);
}*/

/**
  * Will return a formatted size with the unit prefix and one digit following the point.
  * For example:
  * - 1 -> "1 B"
  * - 1024 -> "1.0 KiB"
  * - 1024^2 -> "1.0 MiB"
  * - 1024^3 -> "1.0 GiB"
  * - 1024^4 -> "1.0 TiB"
  * - etc . . . to ZiB
  * The speed of this implementation is equal to the old above : ~1 µs per call (measured with 1 millions calls in release (-O2)).
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
            QString::number(bytes <= 0 ? 0 : bytes).append(" ").append(Constants::BINARY_PREFIXES[i]) :
            QString::number((double)bytes / size, 'f', precision).append(" ").append(Constants::BINARY_PREFIXES[i]);
   }
   return QString();
}

int Global::nbChunks(qint64 fileSize)
{
   return (fileSize + Common::Constants::CHUNK_SIZE - 1) / Common::Constants::CHUNK_SIZE;
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
   static const char units[7] = {'y', 'M', 'w', 'd', 'h', 'm', 's'};

   /* years   */ ymwdhms[0] = seconds / (60 * 60 * 24 * 7 * 4 * 12);
   /* months  */ ymwdhms[1] = seconds / (60 * 60 * 24 * 7 * 4) - (12 * ymwdhms[0]);
   /* weeks   */ ymwdhms[2] = seconds / (60 * 60 * 24 * 7) - (4 * 12 * ymwdhms[0] + 4 * ymwdhms[1]);
   /* days    */ ymwdhms[3] = seconds / (60 * 60 * 24) - (7 * 4 * 12 * ymwdhms[0] + 7 * 4 * ymwdhms[1] + 7 * ymwdhms[2]);
   /* hours   */ ymwdhms[4] = seconds / (60 * 60) - (24 * 7 * 4 * 12 * ymwdhms[0] + 24 * 7 * 4 * ymwdhms[1] + 24 * 7 * ymwdhms[2] + 24 * ymwdhms[3]);
   /* minutes */ ymwdhms[5] = seconds / (60) - (60 * 24 * 7 * 4 * 12 * ymwdhms[0] + 60 * 24 * 7 * 4 * ymwdhms[1] + 60 * 24 * 7 * ymwdhms[2] + 60 * 24 * ymwdhms[3] + 60 * ymwdhms[4]);
   /* seconds */ ymwdhms[6] = seconds  -  (60 * 60 * 24 * 7 * 4 * 12 * ymwdhms[0] + 60 * 60 * 24 * 7 * 4 * ymwdhms[1] + 60 * 60 * 24 * 7 * ymwdhms[2] + 60 * 60 * 24 * ymwdhms[3] + 60 * 60 * ymwdhms[4] + 60 * ymwdhms[5]);

   for (int i = 0; i < 7; i++)
   {
      bool exit = !output.isEmpty();
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
   QString formattedIP;
   if (address.protocol() == QAbstractSocket::IPv4Protocol)
      formattedIP.append(address.toString());
   else
      formattedIP.append("[").append(address.toString()).append("]");
   formattedIP.append(":").append(QString::number(port));
   return formattedIP;
}

/**
  * Return the remaining free space for the given path.
  * On macOS, paths not yet created use the nearest existing parent directory.
  * A failed query returns the maximum qint64 (unknown space).
  */
qint64 Global::availableDiskSpace(const QString& path)
{
   Q_ASSERT(!path.isEmpty());

   QString pathToDir = path;
   QFileInfo fileInfo(path);
   if (fileInfo.isFile())
      pathToDir = fileInfo.absolutePath();

#if defined(Q_OS_WIN32)
   ULARGE_INTEGER space;
   if (!GetDiskFreeSpaceEx(StringUtils::towcharList(pathToDir).constData(), &space, NULL, NULL))
      return std::numeric_limits<qint64>::max();
   return space.QuadPart;
#elif defined(Q_OS_LINUX) || defined(Q_OS_DARWIN)
#ifdef Q_OS_DARWIN
   // Darwin's statvfs has 32-bit block counts even in 64-bit builds. statfs
   // retains the full counts needed for large volumes.
   struct statfs info;
   int result = statfs(pathToDir.toUtf8().constData(), &info);
   // Settings and download destinations may not exist yet. Only ENOENT is
   // recoverable this way: permission/I/O errors must retain the unknown-space
   // fallback rather than accidentally reporting space on a different volume.
   while (result != 0 && errno == ENOENT)
   {
      const QString parent = QFileInfo(pathToDir).absolutePath();
      if (parent == pathToDir)
         break;
      pathToDir = parent;
      result = statfs(pathToDir.toUtf8().constData(), &info);
   }
#else
   struct statvfs info;
   const int result = statvfs(pathToDir.toUtf8().constData(), &info);
#endif
   if (result == 0)
   {
      // f_bavail excludes reserved blocks. Use the allocation unit, not the
      // preferred I/O size (statfs.f_iosize / statvfs.f_bsize).
#ifdef Q_OS_DARWIN
      const quint64 fragmentSize = info.f_bsize;
#else
      const quint64 fragmentSize = info.f_frsize;
#endif
      const quint64 availableBlocks = info.f_bavail;
      const auto maximum = std::numeric_limits<qint64>::max();
      if (fragmentSize != 0 && availableBlocks > quint64(maximum) / fragmentSize)
         return maximum;
      return static_cast<qint64>(fragmentSize * availableBlocks);
   }
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
   const QString existingPath = toWin32LongPath(existingFile);
   const QString newPath = toWin32LongPath(newFile);
   return MoveFileExW(reinterpret_cast<LPCWSTR>(existingPath.utf16()), reinterpret_cast<LPCWSTR>(newPath.utf16()), MOVEFILE_REPLACE_EXISTING);
#else
   return std::rename(existingFile.toUtf8().constData(), newFile.toUtf8().constData()) == 0;
#endif
}

#ifdef Q_OS_WIN32
/**
  * Return the extended-length form of an absolute path, for example "\\?\D:\dir\file" or "\\?\UNC\server\share\file".
  * Win32 functions like 'CreateFileW(..)' fail on paths longer than 'MAX_PATH' (260) without this prefix,
  * while 'QFile' succeeds because Qt adds it itself. Relative paths are only converted to native separators.
  */
QString Global::toWin32LongPath(const QString& path)
{
   const QString native = QDir::toNativeSeparators(path);
   if (native.startsWith(R"(\\?\)") || native.startsWith(R"(\\.\)"))
      return native;

   // The prefix disables the Win32 path normalization: '.' and '..' must be resolved here.
   const QString cleaned = QDir::toNativeSeparators(QDir::cleanPath(path));
   if (cleaned.startsWith(R"(\\)"))
      return R"(\\?\UNC\)" + cleaned.mid(2);
   if (cleaned.size() >= 3 && cleaned[1] == ':' && cleaned[2] == '\\')
      return R"(\\?\)" + cleaned;
   return native;
}
#endif

bool Global::isLocal(const QHostAddress& address)
{
   return address == QHostAddress::LocalHost || address == QHostAddress::LocalHostIPv6 || QNetworkInterface::allAddresses().contains(address);
}

QString Global::dataFolders[2]; // The two folders (roaming and local), see DataFolderType enum.

#ifdef Q_OS_DARWIN
namespace
{
   QString macApplicationFolder(const QString& basePath, bool create)
   {
      if (basePath.isEmpty() || !QDir::isAbsolutePath(basePath))
         throw Global::UnableToGetFolder("Unable to locate the user application directory");
      const QString folder = QDir(basePath).absoluteFilePath(Constants::APPLICATION_FOLDER_NAME);
      if (create && !QDir().mkpath(folder))
         throw Global::UnableToGetFolder(QString("Unable to create the directory %1").arg(folder));
      return folder;
   }
}
#endif

/**
  * Returns the absolute path to the requested data folder.
  * For example under Windows :
  * - type == ROAMING : "C:/Users/john/AppData/Roaming/D-LAN"
  * - type == LOCAL : "C:/Users/john/AppData/Local/D-LAN"
  * On Linux, ROAMING uses $XDG_CONFIG_HOME/d-lan (default ~/.config/d-lan)
  * and LOCAL uses $XDG_DATA_HOME/d-lan (default ~/.local/share/d-lan).
  * On macOS, both use ~/Library/Application Support/D-LAN. Configuration and
  * persistent local state must survive deletion of disposable caches.
  * Creates the folder if requested.
  * @exception UnableToGetFolder
  */
QString Global::getDataFolder(DataFolderType type, bool create)
{
   if (!Global::dataFolders[static_cast<int>(type)].isEmpty())
   {
      if (create)
         QDir::current().mkpath(Global::dataFolders[static_cast<int>(type)]);
      return Global::dataFolders[static_cast<int>(type)];
   }
   else
   {
#ifdef Q_OS_WIN32
      wchar_t dataPath[MAX_PATH];

      if (!SUCCEEDED(SHGetFolderPath(NULL, type == DataFolderType::ROAMING ? CSIDL_APPDATA : CSIDL_LOCAL_APPDATA, NULL, 0, dataPath)))
         throw UnableToGetFolder(QString("Unable to get the %1: SHGetFolderPath failed").arg(type == DataFolderType::ROAMING ? "roaming user directory path" : "local user directory path"));

      const QString dataFolderPath = QString::fromWCharArray(dataPath);
      const QDir dataFolder(dataFolderPath);

      if (create && !dataFolder.exists(Constants::APPLICATION_FOLDER_NAME))
         if (!dataFolder.mkdir(Constants::APPLICATION_FOLDER_NAME))
            throw UnableToGetFolder(
               QString("Unable to create the directory %1 in %2")
                  .arg(Constants::APPLICATION_FOLDER_NAME, dataFolder.absolutePath()
               )
            );

      return dataFolder.absoluteFilePath(Constants::APPLICATION_FOLDER_NAME);
#elif defined(Q_OS_LINUX)
      const QString basePath = QStandardPaths::writableLocation(type == DataFolderType::ROAMING
         ? QStandardPaths::GenericConfigLocation : QStandardPaths::GenericDataLocation);
      if (basePath.isEmpty())
         throw UnableToGetFolder("Unable to locate the user data directory");
      const QString folder = QDir(basePath).absoluteFilePath(Constants::APPLICATION_FOLDER_NAME);
      if (create && !QDir().mkpath(folder))
         throw UnableToGetFolder(QString("Unable to create the directory %1").arg(folder));
      return folder;
#elif defined(Q_OS_DARWIN)
      // Use a fixed app directory so GUI, core and tools share the same data.
      return macApplicationFolder(QStandardPaths::writableLocation(QStandardPaths::GenericDataLocation), create);
#else
      if (create && !QDir::home().exists(Constants::APPLICATION_FOLDER_NAME))
         if (!QDir::home().mkdir(Constants::APPLICATION_FOLDER_NAME))
             throw UnableToGetFolder(QString("Unable to create the directory %1 in %2").arg(Constants::APPLICATION_FOLDER_NAME).arg(QDir::home().absolutePath()));

      return QDir::home().absoluteFilePath(Constants::APPLICATION_FOLDER_NAME);
#endif
   }
}

QString Global::getCacheFolder(bool create)
{
#ifdef Q_OS_DARWIN
   if (Global::dataFolders[static_cast<int>(DataFolderType::LOCAL)].isEmpty())
      return macApplicationFolder(QStandardPaths::writableLocation(QStandardPaths::GenericCacheLocation), create);
#endif
   // Keep command-line overrides and test isolation consistent with LOCAL.
   return Global::getDataFolder(DataFolderType::LOCAL, create);
}

QString Global::getLogFolder(bool create)
{
#ifdef Q_OS_DARWIN
   if (Global::dataFolders[static_cast<int>(DataFolderType::LOCAL)].isEmpty())
   {
      const QString support = QStandardPaths::writableLocation(QStandardPaths::GenericDataLocation);
      if (support.isEmpty() || !QDir::isAbsolutePath(support))
         throw UnableToGetFolder("Unable to locate the user Library directory");
      // Qt has no LogsLocation. On macOS, Logs is a sibling of Application
      // Support in the user Library (also respecting Qt's test-mode root).
      return macApplicationFolder(QDir(QFileInfo(support).absolutePath()).filePath("Logs"), create);
   }
#endif
   return Global::getDataFolder(DataFolderType::LOCAL, create);
}

/**
  * It's possible to override the default data folder for a given type.
  */
void Global::setDataFolder(DataFolderType type, const QString& folder)
{
   if (QDir(folder).exists())
      Global::dataFolders[static_cast<int>(type)] = folder;
}

void Global::setDataFolderToDefault(DataFolderType type)
{
   Global::dataFolders[static_cast<int>(type)].clear();
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

      wchar_t windowsPath_wchar[MAX_PATH];
      if (!SUCCEEDED(SHGetFolderPath(NULL, CSIDL_WINDOWS , NULL, 0, windowsPath_wchar)))
         return QString();
      const QString windowsPath = QString::fromWCharArray(windowsPath_wchar);
      QStringList windowsPathSplit = windowsPath.split('\\');
      if (windowsPathSplit.isEmpty())
         return QString();

      return windowsPathSplit.first().replace('\\', '/') + "/Documents and Settings/LocalService" + (type == DataFolderType::ROAMING ? "/Application Data/" : "/Local Settings/Application Data/") + Constants::APPLICATION_FOLDER_NAME;
   }
#else
   return QString();
#endif
}

/**
  * This method only works on Windows.
  * For example on Windows 7 : "C:\Windows\SysWOW64\config\systemprofile\AppData".
  */
QString Global::getDataSystemFolder(DataFolderType type)
{
#if defined(Q_OS_WIN32)
   wchar_t dataPathSystem[MAX_PATH];
   // SHGetKnownFolderPath should be use for vista a superior but it doesn't exist in mingw.
   if (!SUCCEEDED(SHGetFolderPath(NULL, CSIDL_SYSTEMX86, NULL, 0, dataPathSystem)))
      return QString();
   const QString dataFolderPath = QString::fromWCharArray(dataPathSystem).replace('\\', '/');

   OSVERSIONINFO versionInfo;
   memset(&versionInfo, 0, sizeof(versionInfo));
   versionInfo.dwOSVersionInfoSize = sizeof(versionInfo);
   GetVersionEx(&versionInfo);

   // Vista & Windows 7
   if (versionInfo.dwMajorVersion >= 6)
      return dataFolderPath + "/config/systemprofile/AppData" + (type == DataFolderType::ROAMING ? "/Roaming/" : "/local/") + Constants::APPLICATION_FOLDER_NAME;
   else
      return dataFolderPath + "/config/systemprofile" + (type == DataFolderType::ROAMING ? "/Application Data/" : "/Local Settings/Application Data/") + Constants::APPLICATION_FOLDER_NAME;
#else
   return QString();
#endif
}

// Return the short account name. On macOS this is the effective user running
// the process, including when launched by Finder or as a background service.
QString Global::getCurrentUserName()
{
#if defined(Q_OS_WIN32)
   wchar_t userName[UNLEN + 1]; // UNLEN is from Lmcons.h

   // The size is expected in characters and not in bytes.
   DWORD userNameSize = sizeof(userName) / sizeof(userName[0]);

   // On failure the buffer is left untouched, it must not be read.
   if (!GetUserName(userName, &userNameSize))
      return QString();

   // 'userNameSize' receives the number of characters copied, terminating null character included.
   return QString::fromWCharArray(userName, userNameSize > 0 ? userNameSize - 1 : 0);
#elif defined(Q_OS_LINUX)
   char* login = getlogin();
   if (login)
      return QString::fromUtf8(login);
   else
      return QString();
#elif defined(Q_OS_DARWIN)
   // getlogin() requires a login session and getpwuid() uses shared storage.
   // The reentrant lookup works without a terminal and is safe across threads.
   QByteArray buffer(1024, Qt::Uninitialized);
   const uid_t uid = geteuid();
   constexpr qsizetype maximumBufferSize = 1024 * 1024;
   for (;;)
   {
      struct passwd account {};
      struct passwd* result = nullptr;
      const int error = getpwuid_r(uid, &account, buffer.data(), buffer.size(), &result);
      if (error == 0)
         return result && result->pw_name ? QString::fromUtf8(result->pw_name) : QString();
      if (error != ERANGE || buffer.size() >= maximumBufferSize)
         return QString();
      buffer.resize(buffer.size() * 2);
   }
#else
   return "Bob";
#endif
}

QString Global::getCurrentMachineName()
{
#if defined(Q_OS_WIN32)
   wchar_t machineName[MAX_COMPUTERNAME_LENGTH + 1];

   // The size is expected in characters and not in bytes.
   DWORD machineNameSize = sizeof(machineName) / sizeof(machineName[0]);

   // On failure the buffer is left untouched, it must not be read.
   if (!GetComputerName(machineName, &machineNameSize))
      return QString();

   // 'machineNameSize' receives the number of characters copied, terminating null character excluded.
   return QString::fromWCharArray(machineName, machineNameSize);
#elif defined(Q_OS_LINUX) || defined(Q_OS_DARWIN)
#ifdef Q_OS_DARWIN
   // Prefer the user-visible name from macOS settings over a DNS hostname.
   if (CFStringRef computerName = SCDynamicStoreCopyComputerName(nullptr, nullptr))
   {
      const QString name = QString::fromCFString(computerName);
      CFRelease(computerName);
      if (!name.isEmpty())
         return name;
   }
#endif
   char machineName[256];
   if (gethostname(machineName, sizeof(machineName)) != 0)
      return QString();

   machineName[sizeof(machineName) - 1] = '\0'; // 'gethostname' may not null terminate a truncated name.
   return QString::fromUtf8(machineName);
#else
   return "Bob";
#endif
}

/**
  * Returns the folders which may be shown as shortcuts by a file browser: on Windows the drive roots
  * ("C:\", "D:\", ...), then the home folder of the current user followed by Windows Explorer "Quick Access"
  * folders or Linux/macOS standard user folders.
  * QStandardPaths supplies native paths and display names on macOS and honours the XDG user
  * directory configuration on Linux.
  * Missing folders and duplicates are removed; the home folder is always first on Linux/macOS.
  * @remarks When the core runs as a service the quick access folders are the ones of the service account,
  *          thus there is usually none and only the home folder is returned.
  */
QList<Global::QuickAccessFolder> Global::getQuickAccessFolders()
{
   QList<QuickAccessFolder> folders;
   QSet<QString> knownPaths;

   auto append = [&folders, &knownPaths](const QString& name, const QString& path)
   {
      if (name.isEmpty() || path.isEmpty())
         return;

      const QString cleanedPath = QDir::cleanPath(QDir::fromNativeSeparators(path));

      // Also discards the pinned folders which don't exist anymore.
      if (!QDir::isAbsolutePath(cleanedPath) || !QFileInfo(cleanedPath).isDir())
         return;

      QString pathKey = cleanedPath;
#ifdef Q_OS_WIN32
      pathKey = pathKey.toLower();
#endif
      if (knownPaths.contains(pathKey))
         return;

      knownPaths.insert(pathKey);
      folders << QuickAccessFolder { name, cleanedPath };
   };

#ifdef Q_OS_WIN32
   // The drives which aren't ready, for example an optical drive without disc, are discarded by 'append'.
   for (const QFileInfo& drive : QDir::drives())
      append(QDir::toNativeSeparators(drive.absolutePath()), drive.absolutePath());
#endif

   const QString homePath = QDir::homePath();
   const QString homeName = QDir(homePath).dirName();
   append(homeName.isEmpty() ? QStandardPaths::displayName(QStandardPaths::HomeLocation) : homeName, homePath);

#ifdef Q_OS_WIN32
   // 'CoInitializeEx' returns 'S_FALSE' if COM has already been initialized for this thread, 'CoUninitialize' must be
   // called anyway to balance the calls. It returns 'RPC_E_CHANGED_MODE' if COM has already been initialized with
   // another threading model, in this case COM can be used but must not be uninitialized here.
   const HRESULT COMResult = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE);
   if (FAILED(COMResult) && COMResult != RPC_E_CHANGED_MODE)
      return folders;

   IShellItem* quickAccess = nullptr;
   if (SUCCEEDED(SHCreateItemFromParsingName(L"shell:::{679F85CB-0220-4080-B29B-5540CC05AAB6}", nullptr, IID_PPV_ARGS(&quickAccess))))
   {
      IEnumShellItems* shellItems = nullptr;
      if (SUCCEEDED(quickAccess->BindToHandler(nullptr, BHID_EnumItems, IID_PPV_ARGS(&shellItems))))
      {
         IShellItem* shellItem = nullptr;
         while (shellItems->Next(1, &shellItem, nullptr) == S_OK)
         {
            wchar_t* path = nullptr;
            wchar_t* name = nullptr;

            // The quick access also contains the recent files, only the folders are kept.
            // 'GetAttributes' returns 'S_FALSE' when the asked attributes aren't all set, thus the mask must be tested.
            SFGAOF attributes = 0;
            shellItem->GetAttributes(SFGAO_FOLDER, &attributes);

            // 'SIGDN_FILESYSPATH' fails for the items which aren't real folders, for example the recycle bin, they are skipped.
            if (attributes & SFGAO_FOLDER && SUCCEEDED(shellItem->GetDisplayName(SIGDN_FILESYSPATH, &path)))
            {
               if (SUCCEEDED(shellItem->GetDisplayName(SIGDN_NORMALDISPLAY, &name)))
               {
                  append(QString::fromWCharArray(name), QString::fromWCharArray(path));
                  CoTaskMemFree(name);
               }
               CoTaskMemFree(path);
            }

            shellItem->Release();
         }

         shellItems->Release();
      }

      quickAccess->Release();
   }

   if (COMResult != RPC_E_CHANGED_MODE)
      CoUninitialize();
#elif defined(Q_OS_LINUX) || defined(Q_OS_MACOS)
   for (const auto location : {
      QStandardPaths::DesktopLocation,
      QStandardPaths::DocumentsLocation,
      QStandardPaths::DownloadLocation,
      QStandardPaths::MusicLocation,
      QStandardPaths::PicturesLocation,
      QStandardPaths::MoviesLocation,
      QStandardPaths::PublicShareLocation,
      QStandardPaths::TemplatesLocation
   })
   {
      const QString path = QStandardPaths::writableLocation(location);
      // Qt's macOS displayName() assumes a nonempty standardLocations() list.
      // Unsupported locations (such as Templates) must be skipped before calling it.
      if (!path.isEmpty())
         append(QStandardPaths::displayName(location), path);
   }
#endif

   return folders;
}

/**
  * Create a file containing its name. Parents directories are created if needed.
  * For testing purpose.
  * @return true if the file has been created successfully or false if an error has occurred.
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

   // Remove immediate children recursively, keeping 'dir' itself. Include
   // hidden entries and unlink symlinks without traversing their targets.
   const auto entries = QDir(dir).entryInfoList(QDir::AllEntries | QDir::Hidden | QDir::System | QDir::NoDotAndDotDot);
   for (const QFileInfo& entry : entries)
   {
      const bool removed = entry.isDir() && !entry.isSymLink()
         ? QDir(entry.absoluteFilePath()).removeRecursively()
         : QFile::remove(entry.absoluteFilePath());
      if (!removed)
         success = false;
   }

   return success;
}

/**
  * For testing purpose.
  */
bool Global::recursiveDeleteDirectory(const QString& dir)
{
   Q_ASSERT(!dir.isEmpty());

   bool success = Global::recursiveDeleteDirectoryContent(dir);

   if (QDir::current().exists(dir) && !QDir().rmdir(QDir(dir).absolutePath()))
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

QString Global::getQObjectHierarchy(const QObject* root)
{
   return getQObjectHierarchy(root, [](const QObject* obj) {
      return QString("\"").append(obj->objectName()).append("\" of type ").append(obj->metaObject()->className());
   });
}

/**
  * Can be specialized with a function ('fun') to specify how to transform each object in string.
  */
QString Global::getQObjectHierarchy(const QObject* root, std::function<QString(const QObject*)> fun)
{
   static const int INDENTATION = 3;
   struct Node
   {
      int level;
      const QObject* obj;
   };

   QString result;
   QList<Node> nodesToProcess { Node { 0, root } };

   while (!nodesToProcess.isEmpty())
   {
      Node current = nodesToProcess.takeFirst();
      result.append(QString().fill(' ', INDENTATION * current.level));
      result.append(fun(current.obj)).append('\n');

      QListIterator<QObject*> i(current.obj->children());
      i.toBack();
      while (i.hasPrevious())
         nodesToProcess.prepend(Node { current.level + 1, i.previous() });
   }

   return result;
}

QString Global::getResourceFolder()
{
   const QString executableDirectory = QCoreApplication::applicationDirPath();
#ifdef Q_OS_DARWIN
   QDir bundle(executableDirectory);
   if (bundle.dirName() == "MacOS" && bundle.cdUp() && bundle.dirName() == "Contents" &&
       bundle.exists("Info.plist") && bundle.exists("Resources"))
      return bundle.filePath("Resources");
#endif
   return executableDirectory;
}
