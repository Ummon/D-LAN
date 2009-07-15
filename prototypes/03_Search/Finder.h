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
   /**
     * Creates a new Finder.
     * @param path The path from which all future search will begin.
     */
   Finder(const QDir& path);
    
   /**
     * Search a file or a dire from a pattern.
     * @param pattern The globbing pattern.
     */
   void search(const QString& pattern);
    
   // TODO
   // void nativeSearch(const QString& pattern);
    
   /**
     * Returns the results from the previous search.
     */
   const QStringList& getResults();
    
private:
   const QDir& path;
   QRegExp regexp; ///< Used to match filename against pattern.
   QStringList result;
};

#endif // FINDER_H
