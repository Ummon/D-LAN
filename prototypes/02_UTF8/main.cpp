#include <QtCore/QCoreApplication>
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <QtCore/QTextStream>

#define OUTPUT_DIR "output"
#define INPUT_DIR "input"

int main(int argc, char *argv[])
{
   QTextStream out(stdout);
   
   QDir curDir = QDir::current();
   if (!curDir.exists(OUTPUT_DIR))
      curDir.mkdir(OUTPUT_DIR);
   
   foreach (QString entry, QDir(INPUT_DIR).entryList())
   {
      if (entry == "." ||entry == "..")
         continue;
      
      out << entry << endl;
      
      QFile file(QString(OUTPUT_DIR) + "/" + entry);
      file.open(QIODevice::WriteOnly);
   }
   
   return 0;
}
