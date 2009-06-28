#include "Finder.h"

#include <QtCore/QFileInfo>
#include <QtCore/QFileInfoList>
#include <QtCore/QLinkedList>
#include <QtCore/QDebug>

Finder::Finder(const QDir& path)
   : path(path)
{
   this->regexp.setPatternSyntax(QRegExp::Wildcard);
}

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

const QStringList& Finder::getResults()
{
   return this->result;
}
