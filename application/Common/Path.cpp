#include <Path.h>
using namespace Common;

#include <QDir>

/**
  * @class Path
  * A utility class to represent a path. It can be a file or a directory.
  * The path can be absolute or relative.
  */

Path::Path(const QString& path)
{
   QString cleanedPath = QDir::cleanPath(path);

   if (cleanedPath.isEmpty())
      return;

   // Absolutes cases:
   // Linux.
   if (cleanedPath[0] == '/')
   {
      this->root = "/";
      cleanedPath.remove(0, 1);
   }
   // Windows.
   else if (isWindowsPath(cleanedPath))
   {
      this->root = cleanedPath.left(3);
      cleanedPath.remove(0, 3);
   }
   // Else case: an relative path.

   const bool isDir = path.endsWith('/');

   QStringList names = cleanedPath.split('/', QString::SkipEmptyParts);
   for (int i = 0; i < names.size(); i++)
   {
      if (!isDir && i == names.size() - 1)
         this->filename = names[i];
      else
         this->dirs.append(names[i]);
   }
}

Path::Path(const QString& root, const QStringList& dirs, const QString& filename)
   : root(root), dirs(dirs), filename(filename)
{
}

QString Path::getPath() const
{
   return this->root + this->dirs.join('/') + (this->isAbsolute() && !this->dirs.isEmpty() || !this->dirs.isEmpty() ? "/" : "") + this->filename;
}

bool Path::isFile() const
{
   return !this->filename.isEmpty();
}

bool Path::isAbsolute() const
{
   return !this->root.isEmpty();
}

QString Path::getRoot() const
{
   return this->root;
}

QStringList Path::getDirs() const
{
   return this->dirs;
}

QString Path::getFilename() const
{
   return this->filename;
}

QString Path::getExtension() const
{
   if(!this->isFile())
      return QString();

   const int dotPosition = this->filename.lastIndexOf('.');
   if (dotPosition == 0 || dotPosition == -1 || dotPosition == this->filename.size() - 1)
      return QString();

   return this->filename.right(this->filename.size() - 1 - dotPosition);
}

/**
  * 'this' can be a file but other can't.
  * Both paths must be absolute.
  */
bool Path::isSubOf(const Path& other) const
{
   if (!this->isAbsolute() || !other.isAbsolute())
      return false;

   if (this->getRoot() != other.getRoot())
      return false;

   if (other.isFile())
      return false;

   const auto& thisDirs = this->getDirs();
   const auto& otherDirs = other.getDirs();

   if (thisDirs.size() <= otherDirs.size())
      return false;

   for (int i = 0; i < otherDirs.size(); i++)
      if (thisDirs[i] != otherDirs[i])
         return false;

   return true;
}

/**
  * Both paths can be directory or file.
  * Both paths must be absolute.
  */
bool Path::isSuperOf(const Path& other) const
{
   return other.isSubOf(*this);
}

/**
  * Both paths can be directory or file.
  * Both paths must be absolute.
  */
bool Path::isSameDir(const Path& other) const
{
   if (!this->isAbsolute() || !other.isAbsolute())
      return false;

   if (this->getRoot() != other.getRoot())
      return false;

   return this->getDirs() == other.getDirs();
}

bool Path::operator==(const Path& other) const
{
   return this->getRoot() == other.getRoot() && this->getFilename() == other.getFilename() && this->getDirs() == other.getDirs();
}

Path Path::removeFilename() const
{
   return Path(this->root, this->dirs, QString());
}

Path Path::removeLastDir() const
{
   QStringList dirs = this->dirs;
   if (!dirs.isEmpty())
      dirs.removeLast();
   return Path(this->root, dirs, this->filename);
}

Path Path::setFilename(const QString& filename) const
{
   return Path(this->root, this->dirs, filename);
}

/**
  * Append an other directory.
  * if 'this' is a file, the filename is discared.
  */
Path Path::append(const Common::Path& other) const
{
   QStringList dirs = this->dirs;
   dirs.append(other.dirs);
   return Path(this->root, dirs, other.filename);
}

Path Path::prepend(const Common::Path& other) const
{
   return other.append(*this);
}

Path Path::appendDir(const QString& dir) const
{
   QStringList dirs = this->dirs;
   dirs.append(dir);
   return Path(this->root, dirs, this->filename);
}

Path Path::prependDir(const QString& dir) const
{
   QStringList dirs = this->dirs;
   dirs.prepend(dir);
   return Path(this->root, dirs, this->filename);
}

const QList<QChar> Path::FORBIDDEN_CHARS_IN_PATH { '?', '/', '\\','*', ':', '"', '<', '>', '|' };

/**
  * Replaces forbidden characters from the given path with special token "#n;" where n is the unicode number.
  */
QString Path::sanitizePath(QString path)
{
   for (QListIterator<QChar> i(FORBIDDEN_CHARS_IN_PATH); i.hasNext();)
   {
      const QChar& currentChar = i.next();
      const QString entity = QString("&#").append(QString::number(currentChar.cell())).append(';');
      path.replace(currentChar, entity);
   }
   return path;
}

/**
  * Replace special token with their associated character.
  */
QString Path::unSanitizePath(QString path)
{
   for (QListIterator<QChar> i(FORBIDDEN_CHARS_IN_PATH); i.hasNext();)
   {
      const QChar& currentChar = i.next();
      const QString entity = QString("&#").append(QString::number(currentChar.cell())).append(';');
      path.replace(entity, currentChar);
   }
   return path;
}

/**
  * See QDir::cleanPath(..) documentation.
  * Add a slash at the end.
  */
QString Path::cleanDirPath(const QString& path)
{
   Q_ASSERT(!path.isEmpty());

   QString cleanedPath = QDir::cleanPath(path);
   if (!cleanedPath.isEmpty() && cleanedPath[cleanedPath.size()-1] != '/')
      cleanedPath.append('/');

   return cleanedPath;
}

/**
  * @return 'true' if path begins with  "<Drive letter>:/" or "<Drive letter>:\", for example: "C:/Users/"
  */
bool Path::isWindowsPath(const QString& path)
{
   return path.length() >= 3 && path[0].isLetter() && path[1] == ':' && (path[2] == '/' || path[2] == '\\');
}

/**
  * @return 'true' if path is a Windows root, for example: "C:", "C:\", "C:/"
  */
bool Path::isWindowsRootPath(const QString& path)
{
   return (path.length() == 3 || path.length() == 2) && path[0].isLetter() && path[1] == ':';
}
