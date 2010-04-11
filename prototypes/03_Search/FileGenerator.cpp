#include "FileGenerator.h"

#include <QtCore/QUuid>
#include <QtCore/QDebug>
 
/**
  * @class FileGenerator
  * An instance of this class can generate randomly and recursively some empty files
  * and folders. Theirs names are exactly eight characters long and also randomly generated.
  * This class will be used to test the 'Finder' class.
  */

/**
  * Create some files and some folders into the given directory 'dirname' which is
  * locate in 'current'. It will be created if it doesn't exist.
  * A maximum of 'maxFile'.
  */
void FileGenerator::generate(const QDir& current, const QString& dirname)
{
   this->generate(current, dirname, 0, 0);
}

/**
  * The private recursive version of 'generate'.
  */
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

/**
  * Return a randomly generated eight characters string.
  */
QString FileGenerator::generateName()
{
   return QUuid::createUuid().toString().mid(1, 8);
}

const int FileGenerator::maxFile = 50000; ///< The max generated files.
const int FileGenerator::maxSubdirectoryLevel = 4;
