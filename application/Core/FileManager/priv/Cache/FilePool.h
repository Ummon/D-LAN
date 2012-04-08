#ifndef FILEMANAGER_FILEPOOL_H
#define FILEMANAGER_FILEPOOL_H

#include <QObject>
#include <QMutex>
#include <QFile>
#include <QTime>
#include <QTimer>

namespace FM
{
   class FilePool : public QObject
   {
      Q_OBJECT

      static const int TIME_KEEP_FILE_OPEN_MIN = 2000; // [ms].
      static const int TIME_RECHECK_TO_RELEASE = 1000; // [ms].

   public:
      explicit FilePool(QObject *parent = 0);
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
