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
#include <QByteArray>
#include <QFileInfo>
#include <QSet>
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
#ifdef Q_OS_WIN32
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

QString Global::getCurrentUserName()
{
#if defined(Q_OS_WIN32)
   wchar_t userName[UNLEN + 1]; // UNLEN is from Lmcons.h
   DWORD userNameSize = sizeof(userName);
   GetUserName(userName, &userNameSize);
   return QString::fromWCharArray(userName);
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

QString Global::getCurrentMachineName()
{
#if defined(Q_OS_WIN32)
   wchar_t machineName[MAX_COMPUTERNAME_LENGTH + 1];
   DWORD machineNameSize = sizeof(machineName);
   GetComputerName(machineName, &machineNameSize);
   return QString::fromWCharArray(machineName);
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
  * Returns the folders which may be shown as shortcuts by a file browser: the home folder of the current
  * user followed by the folders of the Windows Explorer "Quick Access" ("Home" since Windows 11), the
  * pinned ones as well as the frequently used ones. The duplicates are removed, the home folder is always
  * the first element.
  * @remarks Only implemented for Windows, the other platforms only get the home folder.
  * @remarks When the core runs as a service the quick access folders are the ones of the service account,
  *          thus there is usually none and only the home folder is returned.
  */
QList<Global::QuickAccessFolder> Global::getQuickAccessFolders()
{
   QList<QuickAccessFolder> folders;
   QSet<QString> knownPaths; // Lower case paths, to avoid duplicates.

   auto append = [&folders, &knownPaths](const QString& name, const QString& path)
   {
      if (name.isEmpty() || path.isEmpty())
         return;

      const QString cleanedPath = QDir::cleanPath(QDir::fromNativeSeparators(path));

      // Also discards the pinned folders which don't exist anymore.
      if (!QFileInfo(cleanedPath).isDir())
         return;

      if (knownPaths.contains(cleanedPath.toLower()))
         return;

      knownPaths.insert(cleanedPath.toLower());
      folders << QuickAccessFolder { name, cleanedPath };
   };

   const QString homePath = QDir::homePath();
   append(QDir(homePath).dirName(), homePath);

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

   for (QDirIterator i(dir, QDir::Files, QDirIterator::Subdirectories); i.hasNext();)
   {
      QFile file(i.next());
      if (file.exists() && !file.remove())
         success = false;
   }

   for (QDirIterator i(dir, QDir::AllDirs | QDir::NoDotAndDotDot, QDirIterator::Subdirectories); i.hasNext();)
      QDir(i.next()).rmpath(".");

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
