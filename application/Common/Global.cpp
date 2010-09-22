#include "Global.h"
using namespace Common;

#include <QDir>
#include <QDirIterator>
#include <qDebug>
#include <QtGlobal>

#ifdef Q_OS_WIN32
   #include <windows.h>
#endif

#include <Constants.h>

qint64 Global::availableDiskSpace(const QString& path)
{
   const qint64 max = 9223372036854775807LL;

#ifdef Q_OS_WIN32
   ULARGE_INTEGER space;
   wchar_t buffer[path.size()];

   int l = path.toWCharArray(buffer);
   buffer[l] = 0;

   if (!GetDiskFreeSpaceEx(buffer, &space, NULL, NULL))
      return max;
   return space.QuadPart;
#endif

   // TODO : Linux
   return max;
}

/**
  * @class Global
  * Some generic global functions.
  */

bool Global::createApplicationFolder()
{
   if (!QDir::home().exists(APPLICATION_FOLDER_NAME))
      return QDir::home().mkdir(APPLICATION_FOLDER_NAME);

   return true;
}

/**
  * Create a file and its parent directories if needed.
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

void Global::recursiveDeleteDirectory(const QString& dir)
{
   for (QDirIterator i(dir, QDir::Files, QDirIterator::Subdirectories); i.hasNext();)
      QFile(i.next()).remove();

   for (QDirIterator i(dir, QDir::AllDirs, QDirIterator::Subdirectories); i.hasNext();)
      QDir::current().rmpath(i.next());
}

