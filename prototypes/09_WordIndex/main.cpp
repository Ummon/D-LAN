#include <QtCore/QCoreApplication>
#include <QtCore/QDebug>

#include <WordIndex.h>

int main(int argc, char *argv[])
{
   QCoreApplication a(argc, argv);
    
   WordIndex<int> index;
    
   {
      QList<QString> words;    
      words.append("pouet");    
      index.addItem(words, 1);
   }
   
   {
      QList<QString> words;  
      words.append("poulpe");  
      index.addItem(words, 2);
   }
   
   {
      QList<QString> words;  
      words.append("pou");
      QList<int> items = index.search(words);
      qDebug() << "Items found (" << items.count() << ") :";
      foreach (int item, items)
         qDebug() << item;
   }

   return a.exec();
}
