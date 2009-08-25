#include <QtCore/QCoreApplication>
#include <QtCore/QDir>
#include <QtCore/QLinkedList>
#include <QtCore/QDebug>

#include <WordIndex.h>

void add(WordIndex<QString>& index, const QString& word, const QString& item)
{
   qDebug() << "Add item : " << item << " indexed by " << word;
   
   QList<QString> words;
   words.append(word);
   index.addItem(words, item);
}

void search(WordIndex<QString>& index, const QString& word)
{
   QList<QString> words;  
   words.append(word);
   QList<QString> items = index.search(words);
   qDebug() << "Search " << word << " : items found (" << items.count() << ") :";
   foreach (QString item, items)
      qDebug() << " - " << item;  
}

void rm(WordIndex<QString>& index, const QString& word, const QString& item)
{
   qDebug() << "Remove " << item << " with word " << word;
   QList<QString> words;  
   words.append(word);
   index.rmItem(words, item);
}

/**
  * Index a file by its name.
  */
void indexFile(WordIndex<QString>& index, const QString& path, const QString& fileName)
{
   const static QRegExp regExp("\\W+");
   QString fullPath = path + "/" + fileName;
   QStringList words = fileName.toLower().split(regExp, QString::SkipEmptyParts);
   
   qDebug() << "Index " << fullPath << " by " << words;
   
   index.addItem(words, fullPath);
}

/**
  * Index a directory by its name.
  */
void indexDir(WordIndex<QString>& index, const QString& path, const QString& dirName)
{
   // The directories are threaten like files.
   indexFile(index, path, dirName);
}

void buildIndex(WordIndex<QString>& index, const QString& path)
{
   QLinkedList<QDir> dirsToVisit;
   dirsToVisit.append(path);
   
   int n = 0;
   
   while (!dirsToVisit.isEmpty())
   {
      foreach (QFileInfo entry, dirsToVisit.takeFirst().entryInfoList())
      {
         if (entry.fileName() == "." || entry.fileName() == "..")
            continue;
         
         if (entry.isDir())
         {
            dirsToVisit.append(entry.absoluteFilePath());
            indexDir(index, entry.absoluteFilePath(), entry.fileName());
            n += 1;
         }
         else
         {
            indexFile(index, entry.absoluteFilePath(), entry.fileName());
            n += 1;
         }
      }
   }
   
   qDebug() << n << " items indexed";
}

void test()
{ 
   WordIndex<QString> index;
    
   add(index, "pouet", "1");
   add(index, "poulpe", "2");
   add(index, "pouet", "3");
   add(index, "pou", "4");
   search(index, "pou");
   
   rm(index, "pouet", "1");
   search(index, "pou");
   
   add(index, "pouet", "1");
   add(index, "mais", "5");
   add(index, "maison", "6");
   search(index, "pouet");
   search(index, "maiso");
   
   rm(index, "pouet", "1");
   rm(index, "poulpe", "2");
   rm(index, "pouet", "3");
   rm(index, "pou", "4");
   rm(index, "mais", "5");
   
   // Should be autmatically deleted at the end of the program.
   // rm(index, "maison", 6); 
}

void printUsage(int argc, char *argv[])
{
   QTextStream out(stdout);
   out << "Usage : " << argv[0] << " (test | <directory> <term>*)" << endl
      << " test : run some little tests." << endl
      << " <directory> : will scan recursively the directory and index each file and folder." << endl
      << "  <term>* : A list of optionally term. For each term a search will be done." << endl;
}

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);

   QTextStream in(stdin);
   QTextStream out(stdout);
   
   if (argc >= 2)
   {
      QString arg1 = argv[1];
      if (arg1 == "test")
         test();  
      else
      {
         WordIndex<QString> index;
            
         buildIndex(index, arg1);
         
         forever
         {
            out << "Type a word : ";
            out.flush();
            QString itemToSearch = in.readLine();
            search(index, itemToSearch);
         }
      }
   }
   else
      printUsage(argc, argv);
   
   a.exec();
}
