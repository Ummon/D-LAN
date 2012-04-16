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

#include <Common/Uncopyable.h>

namespace FM
{
   class FilePool : public QObject, Common::Uncopyable
   {
      Q_OBJECT
      static const int TIME_KEEP_FILE_OPEN_MIN = 2000; // [ms].
      static const int TIME_RECHECK_TO_RELEASE = 1000; // [ms].

   public:
      explicit FilePool(QObject* parent = 0);
      ~FilePool();

      QFile* open(const QString& path, QIODevice::OpenMode mode);
      void release(QFile* file, bool forceToClose = false);
      void forceReleaseAll(const QString& path);

   private slots:
      void tryToDeleteReleasedFiles();

   private:
      struct OpenedFile
      {
         OpenedFile(QFile* file, QIODevice::OpenMode mode) : file(file), mode(mode) {}
         QFile* file;
         QIODevice::OpenMode mode;
         QTime releasedTime; // Null if not released.
      };

      QList<OpenedFile> files;
      QMutex mutex;
      QTimer timer;
   };
}

#endif
