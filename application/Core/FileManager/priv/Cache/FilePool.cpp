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
using namespace FM;

#include <QMutexLocker>

#include <priv/Log.h>

/**
  * @class FilePool
  *
  * A file pool keeps a list of opened files ('open(..)').
  * After a file becomes released ('release(..)' and 'forceReleaseAll(..)'), it stays in open state during at least 'TIME_KEEP_FILE_OPEN_MIN' and can be reused via a call to 'open(..)'.
  * After the 'TIME_KEEP_FILE_OPEN_MIN' delay, the released file is deleted in the main Qt loop.
  */

FilePool::FilePool(QObject* parent) :
   QObject(parent)
{
   this->timer.setInterval(TIME_RECHECK_TO_RELEASE);
   connect(&this->timer, &QTimer::timeout, this, &FilePool::tryToDeleteReleasedFiles);
}

FilePool::~FilePool()
{
   QMutexLocker locker(&this->mutex);

   this->timer.stop();

   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
      delete i.next().file;
   this->files.clear();
}

/**
  * @remarks Because the file is kept opened for some time, take care about not opening too many file at the same time.
  * @param path The absolute path to the file.
  * @param mode The open mode.
  * @param[out] fileCreated Optional, only valid in write mode, set to true if the file didn't exist before.
  * @return The handle or a null pointer if error.
  */
QFile* FilePool::open(const QString& path, QIODevice::OpenMode mode, bool* fileCreated)
{
   QMutexLocker locker(&this->mutex);

   if (fileCreated)
      *fileCreated = false;

   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
   {
      OpenedFile& file = i.next();

      if (file.file->fileName() == path && file.mode == mode && file.releasedTime.isValid())
      {
         L_DEBU(QString("FilePool::open(%1, %2): file already in cache").arg(path).arg(mode));
         file.releasedTime.invalidate();
         return file.file;
      }
   }

   QFile* file = new QFile(path);

   if (fileCreated && mode.testFlag(QIODevice::WriteOnly) && !file->exists())
      *fileCreated = true;

   if (!file->open(mode))
   {      
      if (fileCreated)
         *fileCreated = false;
      delete file;
      return nullptr;
   }

   L_DEBU(QString("FilePool::open(%1, %2): file added to the cache").arg(path).arg(mode));
   this->files << OpenedFile { file, mode, QElapsedTimer() };
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
            locker.unlock(); // The 'delete' below can take a while (because of flushing data), we avoid to block the access to the 'FilePool' by unlocking the mutex.
            delete fileToDelete;
         }
         else
         {
            openedFile.releasedTime.start();
            L_DEBU(QString("FilePool::release(%1, %2): file set as released. Timer already started? : %3").arg(file->fileName()).arg(forceToClose).arg(this->timer.isActive()));
            if (!this->timer.isActive())
               QMetaObject::invokeMethod(&this->timer, "start");
         }
         return;
      }
   }
}

void FilePool::forceReleaseAll(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   QList<QFile*> filesToDelete;

   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
   {
      OpenedFile& openedFile = i.next();
      if (openedFile.file->fileName() == path)
      {
         L_DEBU(QString("FilePool::forceReleaseAll(%1): file forced to release and close").arg(path));
         filesToDelete << openedFile.file;
         i.remove();
      }
   }

   if (!filesToDelete.isEmpty())
   {
      locker.unlock(); // The 'delete' below can take a while (because of flushing data), we avoid to block the access to the 'FilePool' by unlocking the mutex.
      for (QListIterator<QFile*> i(filesToDelete); i.hasNext();)
         delete i.next();
   }
}

void FilePool::tryToDeleteReleasedFiles()
{
   QMutexLocker locker(&this->mutex);

   L_DEBU(QString("FilePool::tryToDeleteReleasedFiles(): number of cached file : %1").arg(this->files.size()));

   QList<QFile*> filesToDelete;

   bool stopTimer = true;
   for (QMutableListIterator<OpenedFile> i(this->files); i.hasNext();)
   {
      const OpenedFile& openedFile = i.next();
      if (openedFile.releasedTime.isValid())
      {
         if (openedFile.releasedTime.elapsed() > TIME_KEEP_FILE_OPEN_MIN)
         {
            L_DEBU(QString("FilePool::tryToDeleteReleasedFiles(): file closed: %1").arg(openedFile.file->fileName()));
            filesToDelete << openedFile.file;
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

   if (!filesToDelete.isEmpty())
   {
      locker.unlock();
      for (QListIterator<QFile*> i(filesToDelete); i.hasNext();)
         delete i.next();
   }
}
