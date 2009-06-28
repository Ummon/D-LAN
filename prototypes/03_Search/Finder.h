#ifndef FINDER_H
#define FINDER_H

#include <QtCore/QDir>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QRegExp>

/**
  * Class to find a directory or a file recursively
  * from a given path using a Glob pattern.
  */
class Finder
{
public:
    Finder(const QDir& path);
    
    void search(const QString& pattern);
    
    // TODO
    // void nativeSearch(const QString& pattern);
    
    const QStringList& getResults();
    
private:
    const QDir& path;
    QRegExp regexp;
    QStringList result;
};

#endif // FINDER_H
