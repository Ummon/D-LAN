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
  
#ifndef FILEMANAGER_FILEPOOL_H
#define FILEMANAGER_FILEPOOL_H

#include <QObject>
#include <QMutex>
#include <QFile>
#include <QTime>
#include <QTimer>
#include <QScopedPointer>

#include <Common/Uncopyable.h>

namespace FM
{
   class FilePool : public QObject, Common::Uncopyable
   {
      Q_OBJECT
      static const int TIME_KEEP_FILE_OPEN_MIN = 2000; // [ms].
      static const int TIME_RECHECK_TO_RELEASE = 1000; // [ms].

   public:
      explicit FilePool(QObject* parent = nullptr);
      ~FilePool();

      QFile* open(const QString& path, QIODevice::OpenMode mode, bool* fileCreated = nullptr);
      void release(QFile* file, bool forceToClose = false);
      void forceReleaseAll(const QString& path);

   private slots:
      void tryToDeleteReleasedFiles();

   private:
      struct OpenedFile
      {
         QFile* file;
         QIODevice::OpenMode mode;
         QTime releasedTime; // Null if not released.
      };

      QList<OpenedFile> files;
      QMutex mutex;
      QTimer timer;
   };

   /**
     * Little helper class to autorelease a file opened with a 'FilePool' when going out of scope.
     * Don't forget to test if the file has been correctely created before using it. For example:
     * AutoReleasedFile f(fp, path, mode);
     * if (!f)
     *    [..]
     */
   class AutoReleasedFile
   {
   public:
      AutoReleasedFile(FilePool& filePool, const QString& path, QIODevice::OpenMode mode, bool forceToClose = false, bool* fileCreated = nullptr) :
         filePool(filePool), file(this->filePool.open(path, mode, fileCreated)), forceToClose(forceToClose) {}

      ~AutoReleasedFile()
      {
         this->filePool.release(this->file, this->forceToClose);
      }

      inline QFile* operator->() const { return this->file; }
      inline QFile& operator*() const { return *this->file; }
      inline bool operator!() const { return !this->file; }

   private:
      FilePool& filePool;
      QFile* file;
      bool forceToClose;
   };
}

#endif
