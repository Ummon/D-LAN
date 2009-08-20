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
   qDebug() << "Search '" << word << "' : items found (" << items.count() << ") :";
   foreach (int item, items)
      qDebug() << " - " << item;  
}

void rm(WordIndex<int>& index, const QString& word, int item)
{
   qDebug() << "Remove " << item << " with word '" << word << "'";
   QList<QString> words;  
   words.append(word);
   index.rmItem(words, item);
}

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);
    
   WordIndex<int> index;
    
   add(index, "pouet", 1);
   add(index, "poulpe", 2);
   search(index, "pou");
   rm(index, "pouet", 1);
   search(index, "pou");
   
   return a.exec();
}
