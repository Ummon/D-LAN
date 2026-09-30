#include <Paths.h>

#include <QDir>

QString StressTests::normalizePath(const QString& path)
{
   QString normalized = QDir::cleanPath(QDir::fromNativeSeparators(path));
   while (normalized.size() > 1 && normalized.endsWith('/'))
      normalized.chop(1);

#if defined(Q_OS_WIN32) || defined(Q_OS_DARWIN)
   return normalized.toLower();
#else
   return normalized;
#endif
}

bool StressTests::isSamePath(const QString& path1, const QString& path2)
{
   return normalizePath(path1) == normalizePath(path2);
}

bool StressTests::isSameOrInside(const QString& path, const QString& directory)
{
   const QString normalizedPath = normalizePath(path);
   const QString normalizedDirectory = normalizePath(directory);
   return normalizedPath == normalizedDirectory || normalizedPath.startsWith(normalizedDirectory + '/');
}
