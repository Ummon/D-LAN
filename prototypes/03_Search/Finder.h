#ifndef FINDER_H
#define FINDER_H

#include <QtCore/QDir>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QRegExp>

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
   QRegExp regexp; ///< Used to match filename against pattern.
   QStringList result;
};

#endif
