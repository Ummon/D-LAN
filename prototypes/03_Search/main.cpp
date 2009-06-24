#include <QtCore/QCoreApplication>
#include <QtCore/QTextStream>
#include <QtCore/QDir>
#include <QtCore/QFile>

#include <FileGenerator.h>

void printUsage(QTextStream& out)
{
   out << "Usage : 03_Search ( generate <dir> | search <term> <dir> )" << endl;
}

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
   else
   {
      printUsage(out);
      return 1;
   }
   
   return 0;
}
