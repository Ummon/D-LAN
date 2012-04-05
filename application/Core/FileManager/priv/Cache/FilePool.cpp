#include <priv/Cache/FilePool.h>
using namespace FM;

#include <QMutexLocker>

#include <priv/Log.h>

/**
  * @class FilePool
  *
  *
  */

FilePool::FilePool(QObject* parent) :
   QObject(parent)
{
   this->timer.setInterval(TIME_RECHECK_TO_RELEASE);
   connect(&this->timer, SIGNAL(timeout()), this, SLOT(tryToDeleteReleasedFiles()));
}

FilePool::~FilePool()
{
   QMutexLocker locker(&this->mutex);

   this->timer.stop();

   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
      delete i.next().file;
   this->files.clear();
}

QFile* FilePool::open(const QString& path, QIODevice::OpenMode mode)
{
   QMutexLocker locker(&this->mutex);

   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
   {
      OpenedFile& file = i.next();

      if (file.file->fileName() == path && file.mode == mode && !file.releasedTime.isNull())
      {
         L_DEBU(QString("FilePool::open(%1, %2): file already in cache").arg(path).arg(mode));
         file.releasedTime = QTime();
         return file.file;
      }
   }

   QFile* file = new QFile(path);
   if (!file->open(mode))
   {
      delete file;
      return 0;
   }

   L_DEBU(QString("FilePool::open(%1, %2): file added to the cache").arg(path).arg(mode));
   this->files << OpenedFile(file, mode);
   return file;
}

void FilePool::release(QFile* file, bool forceToClose)
{
   if (!file)
      return;

   QMutexLocker locker(&this->mutex);

   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
   {
      OpenedFile& openedFile = i.next();
      if (openedFile.file == file)
      {
         if (forceToClose)
         {
            L_DEBU(QString("FilePool::release(%1, %2): file forced to close").arg(file->fileName()).arg(forceToClose));
            QFile* fileToDelete = openedFile.file;
            i.remove();
            locker.unlock();
            delete fileToDelete;
         }
         else
         {
            openedFile.releasedTime.start();
            L_DEBU(QString("FilePool::release(%1, %2): file set as released. Timer already started? : %3").arg(file->fileName()).arg(forceToClose).arg(this->timer.isActive()));
            if (!this->timer.isActive())
               QMetaObject::invokeMethod(&this->timer, "start");
         }
         break;
      }
   }
}

void FilePool::forceReleaseAll(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
   {
      OpenedFile& openedFile = i.next();
      if (openedFile.file->fileName() == path)
      {
            L_DEBU(QString("FilePool::forceReleaseAll(%1): file forced to release and close").arg(path));
            delete openedFile.file;
            i.remove();
      }
   }
}

void FilePool::tryToDeleteReleasedFiles()
{
   QMutexLocker locker(&this->mutex);

   L_DEBU(QString("FilePool::tryToDeleteReleasedFiles(): number of cached file : %1").arg(this->files.size()));

   bool stopTimer = true;
   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
   {
      const OpenedFile& openedFile = i.next();
      if (!openedFile.releasedTime.isNull())
      {
         if (openedFile.releasedTime.elapsed() > TIME_KEEP_FILE_OPEN_MIN)
         {
            L_DEBU(QString("FilePool::tryToDeleteReleasedFiles(): file close : %1").arg(openedFile.file->fileName()));
            delete openedFile.file;
            i.remove();
         }
         else
         {
            stopTimer = false;
         }
      }
   }

   if (stopTimer)
   {
      L_DEBU("FilePool::tryToDeleteReleasedFiles(): timer stopped");
      this->timer.stop();
   }
}
