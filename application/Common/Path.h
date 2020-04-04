#pragma once

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
      Path(const QString&& root, const QStringList&& dirs, const QString&& filename);

   public:
      /**
        * Construct a null path.
        */
      Path() = default;
      ~Path() = default;

      Path(const Path& other) = default;
      Path(Path&& other) = default;

      Path& operator=(const Path&) = default;
      Path& operator=(Path&&) = default;

      QString getPath() const;
      bool isFile() const;
      bool isAbsolute() const;
      bool isNull() const;

      QString getRoot() const;
      QStringList getDirs() const;
      QString getFilename() const;
      QString getExtension() const;

      bool isSubOf(const Path& other) const;
      bool isSuperOf(const Path& other) const;
      bool isSameDir(const Path& other) const;
      bool operator==(const Path& other) const;

      Path removeFilename() const &;
      Path removeFilename() &&;

      Path removeLastDir() const &;
      Path removeLastDir() &&;

      Path setFilename(const QString& filename) const &;
      Path setFilename(QString&& filename) &&;

      Path append(const Common::Path& other) const &;
      Path append(Common::Path&& other) &&;

      Path prepend(const Common::Path& other) const &;
      Path prepend(Common::Path&& other) &&;

      Path appendDir(const QString& dir) const &;
      Path appendDir(const QString& dir) &&;

      Path prependDir(const QString& dir) const &;
      Path prependDir(const QString& dir) &&;

      // Helpers.

      static const QList<QChar> FORBIDDEN_CHARS_IN_PATH;
      static QString sanitizePath(QString filename);
      static QString unSanitizePath(QString filename);

      static QString cleanDirPath(const QString& path);
      static bool isWindowsPath(const QString& path);
      static bool isWindowsRootPath(const QString& path);

   private:
      QString root; // For example: Windows: "C:/", Linux: "/".
      QStringList dirs;
      QString filename; // Empty if file.
   };
}
