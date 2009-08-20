#include <QtCore/QCoreApplication>
#include <QtCore/QDebug>

#include <WordIndex.h>

void add(WordIndex<int>& index, const QString& word, int n)
{
   qDebug() << "Add item : " << n << " indexed by " << word;
   
   QList<QString> words;
   words.append(word);
   index.addItem(words, n);
}

void search(WordIndex<int>& index, const QString& word)
{
   QList<QString> words;  
   words.append(word);
   QList<int> items = index.search(words);
   qDebug() << "Search " << word << " : items found (" << items.count() << ") :";
   foreach (int item, items)
      qDebug() << " - " << item;  
}

void rm(WordIndex<int>& index, const QString& word, int item)
{
   qDebug() << "Remove " << item << " with word " << word;
   QList<QString> words;  
   words.append(word);
   index.rmItem(words, item);
}

void test()
{ 
   WordIndex<int> index;
    
   add(index, "pouet", 1);
   add(index, "poulpe", 2);
   add(index, "pouet", 3);
   add(index, "pou", 4);
   search(index, "pou");
   
   rm(index, "pouet", 1);
   search(index, "pou");
   
   add(index, "pouet", 1);
   add(index, "mais", 5);
   add(index, "maison", 6);
   search(index, "pouet");
   search(index, "maiso");
   
   rm(index, "pouet", 1);
   rm(index, "poulpe", 2);
   rm(index, "pouet", 3);
   rm(index, "pou", 4);
   rm(index, "mais", 5);
   
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
   printUsage(argc, argv);
   test();
}
