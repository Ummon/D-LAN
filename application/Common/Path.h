#ifndef COMMON_PATH_H
#define COMMON_PATH_H

#include <QChar>
#include <QString>
#include <QStringList>

namespace Common
{
   class Path
   {
   public:
      Path(const QString& path);

      QString getPath() const;
      bool isFile() const;
      bool isAbsolute() const;

      QString getRoot() const;
      QStringList getDirs() const;
      QString getFilename() const;
      QString getExtension() const;

      static const QList<QChar> FORBIDDEN_CHARS_IN_PATH;
      static QString sanitizePath(QString filename);
      static QString unSanitizePath(QString filename);

      static QString cleanDirPath(const QString& path);
      static bool isWindowsPath(const QString& path);
      static bool isWindowsRootPath(const QString& path);

   private:
      QString root;
      QStringList dirs;
      QString filename;
   };
}

#endif
