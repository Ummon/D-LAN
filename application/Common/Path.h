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
      // Separators, null characters and drive roots such as 'C:' are rejected
      // with std::invalid_argument.
      // Empty components are ignored; '.' and '..' are normalized.
      explicit Path(const QList<QString>& dirs);

   private:
      Path(const QString& root, const QStringList& dirs, const QString& filename);
      Path(QString&& root, QStringList&& dirs, QString&& filename);

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

      QString toString(bool withFilename = true) const;
      bool isFile() const;
      bool isAbsolute() const;
      bool isNull() const;

      QString getRoot() const;
      QStringList getDirs() const;
      QString getLastDir() const;
      QString getLastElement(bool includeRoot = false) const;
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

      Path removeLastElement() const&;
      Path removeLastElement() &&;

      // A single filename, or empty to remove it. Separators, null characters, '.' and '..'
      // are rejected with std::invalid_argument.
      Path setFilename(const QString& filename) const &;
      Path setFilename(QString&& filename) &&;

      // Combine paths and normalize '.' and '..', stopping at an absolute root.
      // append keeps this root and the other filename; prepend does the reverse.
      Path append(const Common::Path& other) const &;
      Path append(Common::Path&& other) &&;

      Path prepend(const Common::Path& other) const &;
      Path prepend(Common::Path&& other) &&;

      // Single directory components, with the same rules as the list constructor.
      Path appendDir(const QString& dir) const &;
      Path appendDir(const QString& dir) &&;

      Path prependDir(const QString& dir) const &;
      Path prependDir(const QString& dir) &&;

      operator QString() const;

      // Helpers.
      static const QList<QChar> FORBIDDEN_CHARS_IN_PATH;
      static QString sanitizePath(QString filename);
      static QString unSanitizePath(QString filename);

      static Common::Path fromExistingPath(const QString& path);

      // static QString cleanDirPath(const QString& path);
      static bool isWindowsPath(const QString& path);
      static bool isWindowsRootPath(const QString& path);

   private:
      void normalizeDirs();

      QString root; // For example: Windows: "C:/", UNC: "//server/share/", Linux: "/".
      QStringList dirs; // Can be empty.
      QString filename; // Empty if directory.
   };
}
