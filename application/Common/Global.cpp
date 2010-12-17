#include <Common/Global.h>
using namespace Common;

#include <QDir>
#include <QDirIterator>
#include <QDebug>
#include <QtGlobal>

#ifdef Q_OS_WIN32
   #include <windows.h>
#else
   #include <cstdio>
#endif

#include <Constants.h>

/**
  * @class Global
  * Some generic global functions.
  */

Global::UnableToSetTempDirException::UnableToSetTempDirException(const QString& dir)
   : errorMessage(QString("Unable to create the temporary directory %1").arg(dir).toUtf8())
{
}

const char* Global::UnableToSetTempDirException::what() const throw()
{
   return this->errorMessage.constData();
}

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
  * Rename a file, if 'newFile' already exists, it will be replaced by 'existingFile'.
  * @remarks Qt doesn't offer any way to replace a file by an other in one operation.
  * @return false if the rename didn't work.
  */
bool Global::rename(const QString& existingFile, const QString& newFile)
{
#ifdef Q_OS_WIN32
   return MoveFileEx((LPCTSTR)existingFile.utf16(), (LPCTSTR)newFile.utf16(), MOVEFILE_REPLACE_EXISTING);
#else
   return std::rename(qPrintable(existingFile), qPrintable(newFile));
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
  * For testing purpose.
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

/**
  * For testing purpose.
  */
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
/**
  * For testing purpose.
  */
bool Global::recursiveDeleteDirectory(const QString& dir)
{
   bool success = Global::recursiveDeleteDirectoryContent(dir);

   if (QDir::current().exists(dir) && !QDir(dir).rmdir("."))
      success = false;

   return success;
}

/**
  * Create a directory into the temp directory and set as the current one.
  * For testing purpose.
  */
QString Global::setCurrentDirToTemp(const QString& dirname)
{
   const QString TEMP_DIRNAME("Aybabtu" + dirname);
   QDir::setCurrent(QDir::tempPath());
   if (!QDir::current().exists(TEMP_DIRNAME))
      if (!QDir::current().mkdir(TEMP_DIRNAME))
         throw UnableToSetTempDirException(QDir(TEMP_DIRNAME).absolutePath());

   QDir dir;
   dir.cd(TEMP_DIRNAME);
   QDir::setCurrent(dir.absolutePath());
   return dir.absolutePath();
}

