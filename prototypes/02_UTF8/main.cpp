#include <QtCore/QCoreApplication>
#include <QtCore/QFile>
#include <QtCore/QDir>
#include <QtCore/QTextStream>
#include <QtCore/QTextCodec>

static const QString OUTPUT_DIR("output");
static const QString INPUT_DIR("input");

/**
  * Create a new directory in the current one.
  * If it already exists then delete all files in it.
  * @param dirname The name of the new directory.
  */
void createDir(const QString& dirName)
{
   QDir curr = QDir::current();

   if (!curr.exists(dirName))
      curr.mkdir(dirName);
   else
   {
      QDir dir(curr.absolutePath() + "/" + dirName);
      foreach (QString e, dir.entryList())
      {
         QFile::remove(dir.absolutePath() + "/" + e);
      }
   }
}

/**
  * Create and/or empty the input and ouput directories.
  */
void reinitDirs()
{
   createDir(INPUT_DIR);
   createDir(OUTPUT_DIR);
}

/**
  * 'argc' and 'argv' are unused.
  */
int main(int argc, char *argv[])
{
   QTextCodec::setCodecForLocale(QTextCodec::codecForName("UTF-8"));
   
   QTextStream out(stdout);
   
   // Not required because of the call to 'setCodecForLocale' above.
   //out.setCodec("UTF-8");
   
   // Create directory 'input' and 'output' if they doesn't exist.
   reinitDirs();
   
   QString filename = QString::fromUtf8("abc123 èé� @Ã#$ƇȤՖÿ");
   out << filename << endl <<
      "length : " << filename.length() << endl;
   
   QDir input(INPUT_DIR);
   
   {
      QFile f(input.absolutePath() + "/" + filename);
      f.open(QIODevice::WriteOnly);
   }
   
   foreach (QString entry, input.entryList())
   {      
      if (entry == "." || entry == "..")
         continue;
            
      QFile f(QString(OUTPUT_DIR) + "/" + entry);
      f.open(QIODevice::WriteOnly);
   }
   
   out << "You can now compare the files created in both 'input' and 'ouput' folder" << endl;
   
   return 0;
}
