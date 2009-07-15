#include <QtCore/QCoreApplication>
#include <QtCore/QTextStream>
#include <QtCore/QDir>
#include <QtCore/QFile>

#include <FileGenerator.h>
#include <Finder.h>

/**
  * Print the usage of the progam.
  * @param out The information about the usage will be print in this stream.
  */
void printUsage(QTextStream& out)
{
   out << "Usage : 03_Search ( generate <dir> | search <dir> <term> )" << endl;
}

/**
  * The entry point.
  * See 'printUsage' to know the possible entries.
  */
int main(int argc, char* argv[])
{   
   QTextStream out(stdout);  
   
   if (argc <= 2)
   {
      printUsage(out);
      return 1;
   }   
   QString command(argv[1]);
   
   if (command == "generate")
   {
      FileGenerator generator;
      generator.generate(QDir::current(), QString(argv[2]));
   }
   if (command == "search")
   {
      if (argc <= 3)
      {
         printUsage(out);
         return 1;
      }
      
      QDir t = QDir(argv[2]);
      Finder finder(t);
      finder.search(QString(argv[3]));
      
      foreach (QString file, finder.getResults())
         out << file << endl;
   }
   else
   {
      printUsage(out);
      return 1;
   }
   
   return 0;
}
