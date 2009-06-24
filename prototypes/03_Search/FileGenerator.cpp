#include "FileGenerator.h"

#include <QtCore/QUuid>
#include <QtCore/QDebug>
 
QString FileGenerator::generateName()
{
   return QUuid::createUuid().toString().mid(1, 8);
}

void FileGenerator::generate(const QDir& current, const QString& dirname)
{
   this->generate(current, dirname, 0, 0);
}

int FileGenerator::generate(const QDir& current, const QString& dirname, int numOfFile, int level)
{
   if (level >= maxSubdirectoryLevel)
      return numOfFile ;
      
   current.mkdir(dirname);
   
   int toNum = numOfFile + (qrand() % 200 + 20);
   for (;numOfFile < toNum; numOfFile++)
   {
      if (numOfFile > maxFile)
         return -1;
      
      QFile file(current.absolutePath().append("/").append(dirname).append("/").append(this->generateName()));
      file.open(QIODevice::WriteOnly);
   }
   
   int numDir = (qrand() % 40 + 10);
   for (int i = 0; i < numDir; i++)
   {
      numOfFile = this->generate(QDir(current.absolutePath().append("/").append(dirname)), this->generateName(), numOfFile, level + 1);
      if (numOfFile == -1)
         return -1;         
      qDebug() << static_cast<int>((static_cast<double>(numOfFile) / static_cast<double>(FileGenerator::maxFile)) * 100) << "%";
   }
   
   return numOfFile;
}

const int FileGenerator::maxFile = 50000;
const int FileGenerator::maxSubdirectoryLevel = 4;
