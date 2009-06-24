#ifndef FILEGENERATOR_H
#define FILEGENERATOR_H

#include <QtCore/QDir>
#include <QtCore/QFile>

class FileGenerator
{   
public:   
   /**
     * Create some files and some folders into the given directory 'current'.
     * A maximum of 'maxFile'.
     * @current the path to create the directory in it.
     * @dirname the directory to create and to populate.
     * @numOfFile the number of file created. Each time a file is created this variable will be incremented.
     * @level the level of subdirectory for the current directory. The level max is given by 'maxSubdirectoryLevel'.
     */
   int generate(const QDir& current, const QString& dirname, int numOfFile = 0, int level = 0);
   
private :
   /**
     * Return a randomly generated eight characters string.
     */
   QString generateName();

   static const int maxFile;
   static const int maxSubdirectoryLevel;
};

#endif // FILEGENERATOR_H
