#include <QtCore/QCoreApplication>
#include <QtCore/QDir>
#include <QtCore/QLinkedList>
#include <QtCore/QTime>
#include <QtCore/QTextStream>
#include <QtCore/QPair>
#include <QtCore/QDebug>

#include <WordIndex.h>

// When this macro is set to '1', 'int' are used instead of 'QString' when
// indexing a directory. This is useful when doing some measures because in
// a real application the index items will be pointer on a complex item.
#define USE_INT 1

static const int MAX_WORD_LENGTH = 3;

QTextStream in(stdin);
QTextStream out(stdout);

template <typename T>
void add(WordIndex<T>& index, const QString& word, const T& item)
{
   out << "Add item : " << item << " indexed by " << word << endl;

   QList<QString> words;
   words.append(word);
   index.addItem(words, item);
}

template <typename T>
void search(WordIndex<T>& index, const QString& word)
{
   QList<QString> words;
   words.append(word);

   const QTime& t = QTime::currentTime();

   QList<T> items = index.search(words);
   out << "Search \"" << word << "\" : " << items.count() << " items found in " << (double)t.msecsTo(QTime::currentTime()) / 1000 << " s" << endl;
   foreach (T item, items)
      out << " - " << item << endl;
}

template <typename T>
void rm(WordIndex<T>& index, const QString& word, const T& item)
{
   out << "Remove " << item << " with word " << word << endl;
   QList<QString> words;
   words.append(word);
   index.rmItem(words, item);
}

/**
  * Index a file by its name.
  */
template <typename T>
void indexFile(WordIndex<T>& index, const QString& path, const QString& fileName)
{
   const static QRegExp regExp("(\\W+|_)");
   QString fullPath = path + "/" + fileName;
   QStringList words = fileName.toLower().split(regExp, QString::SkipEmptyParts);

   // Remove all word smaller than MAX_WORD_LENGTH.
   int i = 0;
   while (i < words.length())
   {
      if (words[i].length() < MAX_WORD_LENGTH)
         words.removeAt(i);
      else
         i += 1;
   }

   qDebug() << "Index" << fullPath << "by" << words;

#if USE_INT == 1
   static int n = 0;
   index.addItem(words, n++);
#else
   index.addItem(words, fullPath);
#endif
}

/**
  * Index a directory by its name.
  * The directories are threaten like files.
  */
template <typename T>
void indexDir(WordIndex<T>& index, const QString& path, const QString& dirName)
{
   indexFile(index, path, dirName);
}

template <typename T>
void buildIndex(WordIndex<T>& index, const QString& path)
{
   // Directories and files mixed (QPair::first is the location (path), QPair::second is the name of the file/dir).
   // This variable exists only for counting the time without the disk access.
   QLinkedList< QPair< QString, QString > > items;

   QLinkedList<QDir> dirsToVisit;
   dirsToVisit.append(path);
   while (!dirsToVisit.isEmpty())
   {
      foreach (QFileInfo entry, dirsToVisit.takeFirst().entryInfoList())
      {
         if (entry.fileName() == "." || entry.fileName() == "..")
            continue;

         items.append(QPair<QString, QString>(entry.absoluteFilePath(), entry.fileName()));

         if (entry.isDir())
            dirsToVisit.append(entry.absoluteFilePath());
      }
   }

   out << "Indexing..." << endl;
   const QTime& t = QTime::currentTime();

   int n = 0;
   for (QLinkedList< QPair< QString, QString > >::iterator i = items.begin(); i != items.end(); i++)
   {
      n += 1;
      indexFile(index, i->first, i->second);
   }

   out << n << " items indexed in " << (double)t.msecsTo(QTime::currentTime()) / 1000 << " s" << endl;
}

void test()
{
   WordIndex<QString> index;

   add(index, "banana", QString("banana1"));
   add(index, "banana", QString("banana2"));
   add(index, "ban", QString("ban"));
   add(index, "bananasplit", QString("bananasplit"));
   search(index, "bana");

   rm(index, "bananasplit", QString("bananasplit"));
   search(index, "bana");

   add(index, "bananasplit", QString("bananasplit"));
   add(index, "bananas", QString("bananas"));
   search(index, "bana");

   rm(index, "banana", QString("banana1"));
   rm(index, "banana", QString("banana2"));
   rm(index, "ban", QString("ban"));
   rm(index, "bananasplit", QString("bananasplit"));

   // Should be automatically deleted at the end of the program.
   // rm(index, "bananas", QString("bananas"));
}

void printUsage(int argc, char *argv[])
{
   QTextStream out(stdout);
   out << "Usage : " << argv[0] << " <directory>*" << endl
      << " <directory> : will scan recursively the directory and index each file and folder." << endl
      << " If there is no directory given some little tests are executed." << endl;
}

int main(int argc, char *argv[])
{
   if (argc >= 2)
   {
#if USE_INT == 1
      WordIndex<int> index;
#else
      WordIndex<QString> index;
#endif

      for (int i = 1; i < argc; i++)
         buildIndex(index, argv[i]);

      forever
      {
         out << "Type a word : ";
         out.flush();
         QString itemToSearch = in.readLine();
         if (itemToSearch.length() < MAX_WORD_LENGTH)
            out << "The word must have more than 2 letters" << endl;
         else
            search(index, itemToSearch);
      }
   }
   else
      test();
}
