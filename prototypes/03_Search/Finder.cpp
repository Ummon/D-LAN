#include "Finder.h"

#include <QtCore/QFileInfo>
#include <QtCore/QFileInfoList>
#include <QtCore/QLinkedList>
#include <QtCore/QDebug>


/**
  * @class Finder
  * Class to find a directory or a file recursively
  * from a given path using a Glob pattern.
  */

/**
  * Creates a new Finder.
  * 'path' is the path from which all future search will begin.
  */
Finder::Finder(const QDir& path)
   : path(path)
{
   this->regexp.setPatternSyntax(QRegExp::Wildcard);
}


/**
  * Search a file or a dire from a globbing pattern.
  */
void Finder::search(const QString& pattern)
{
   this->regexp.setPattern(pattern);
   this->result.clear();
   
   QLinkedList<QDir> dirsToVisit;
   dirsToVisit.append(this->path);
   
   while (!dirsToVisit.isEmpty())
   {
      foreach (QFileInfo entry, dirsToVisit.takeFirst().entryInfoList())
      {
         if (entry.fileName() == "." || entry.fileName() == "..")
            continue;
         
         if (entry.isDir())
            dirsToVisit.append(entry.absoluteFilePath());
         else
         {
            if (this->regexp.exactMatch(entry.fileName()))
               this->result.append(entry.absoluteFilePath());
         }              
      }
   }
}

/**
  * Returns the results from the previous search.
  */
const QStringList& Finder::getResults()
{
   return this->result;
}
