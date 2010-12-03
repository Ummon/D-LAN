#include <Common/Global.h>
using namespace Common;

#include <QDir>
#include <QDirIterator>
#include <qDebug>
#include <QtGlobal>

#ifdef Q_OS_WIN32
   #include <windows.h>
#endif

#include <Constants.h>

/**
  * @class Global
  * Some generic global functions.
  */

/**
  * The number of k-combinations (each of size k) from a set S with n elements (size n).
  * @link http://en.wikipedia.org/wiki/Combination
  */
int Global::nCombinations(int n, int k)
{
   if (n < 0 || k < 0)
      return 0;

   int c = 1;
   for(int i = 1; i <= k; i++)
      c = c * (n - k + i) / i;
   return c;
}

/**
  * Will return a formated size with the unit prefix and one digit folowing the point.
  * For example :
  * - 1 -> "1 B"
  * - 1024 -> "1.0 KiB"
  * - 1024^2 -> "1.0 MiB"
  * - 1024^3 -> "1.0 GiB"
  * - 1024^4 -> "1.0 TiB"
  * - etc.. to ZiB
  */
QString Global::formatByteSize(qint64 bytes)
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
}

qint64 Global::availableDiskSpace(const QString& path)
{
   const qint64 MAX = 9223372036854775807LL;

#ifdef Q_OS_WIN32
   ULARGE_INTEGER space;
   wchar_t buffer[path.size()];

   int l = path.toWCharArray(buffer);
   buffer[l] = 0;

   if (!GetDiskFreeSpaceEx(buffer, &space, NULL, NULL))
      return MAX;
   return space.QuadPart;
#endif

   // TODO : Linux
   return MAX;
}

/**
  * TODO : Linux
  * Rename a file, if 'newFile' already exists, it will be replaced by 'existingFile'.
  * @remarks Qt doesn't offer any way to replace a file by an other in one operation.
  * @return false if the rename didn't work.
  */
bool Global::rename(const QString& existingFile, const QString& newFile)
{
#ifdef Q_OS_WIN32
   return MoveFileEx((LPCTSTR)existingFile.utf16(), (LPCTSTR)newFile.utf16(), MOVEFILE_REPLACE_EXISTING);
#else
   return false;
#endif
}

bool Global::createApplicationFolder()
{
   if (!QDir::home().exists(APPLICATION_FOLDER_NAME))
      return QDir::home().mkdir(APPLICATION_FOLDER_NAME);

   return true;
}

/**
  * Create a file containing its name. Parents directories are created if needed.
  * For testing puprose.
  */
void Global::createFile(const QString& path)
{
   QFileInfo fileInfo(path);
   QDir::current().mkpath(fileInfo.path());
   if (fileInfo.fileName().isEmpty())
      return;

   QFile file(path);
   file.open(QIODevice::WriteOnly);
   QTextStream stream(&file);
   stream << fileInfo.fileName();
}

bool Global::recursiveDeleteDirectoryContent(const QString& dir)
{
   bool success = true;

   for (QDirIterator i(dir, QDir::Files, QDirIterator::Subdirectories); i.hasNext();)
      if (!QFile(i.next()).remove())
         success = false;

   for (QDirIterator i(dir, QDir::AllDirs | QDir::NoDotAndDotDot, QDirIterator::Subdirectories); i.hasNext();)
      if (!QDir::current().rmdir(i.next()))
         success = false;

   return success;

}

bool Global::recursiveDeleteDirectory(const QString& dir)
{
   bool success = Global::recursiveDeleteDirectoryContent(dir);

   if (QDir::current().exists(dir) && !QDir(dir).rmdir("."))
      success = false;

   return success;
}

