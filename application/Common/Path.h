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

   private:
      Path(const QString& root, const QStringList& dirs, const QString& filename);

   public:
      ~Path() = default;

      Path(const Path& other) = default;
      Path(Path&& other) = default;

      Path& operator=(const Path&) = default;
      Path& operator=(Path&&) = default;

      QString getPath() const;
      bool isFile() const;
      bool isAbsolute() const;

      QString getRoot() const;
      QStringList getDirs() const;
      QString getFilename() const;
      QString getExtension() const;

      bool isSubOf(const Path& other) const;
      bool isSuperOf(const Path& other) const;
      bool isSameDir(const Path& other) const;
      bool operator==(const Path& other) const;

      Path removeFilename() const;
      Path removeLastDir() const;
      Path setFilename(const QString& filename) const;

      Path append(const Common::Path& other) const;
      Path prepend(const Common::Path& other) const;

      Path appendDir(const QString& dir) const;
      Path prependDir(const QString& dir) const;

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
