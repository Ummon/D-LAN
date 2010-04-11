#ifndef FILEGENERATOR_H
#define FILEGENERATOR_H

#include <QtCore/QDir>
#include <QtCore/QFile>

class FileGenerator
{   
public:
   void generate(const QDir& current, const QString& dirname);
   
private :
   int generate(const QDir& current, const QString& dirname, int numOfFile, int level);
   QString generateName();

   static const int maxFile;
   static const int maxSubdirectoryLevel;
};

#endif
