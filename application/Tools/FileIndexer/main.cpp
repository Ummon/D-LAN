/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
#include <QtCore/QCoreApplication>
#include <QtCore/QDir>
#include <QtCore/QLinkedList>
#include <QtCore/QElapsedTimer>
#include <QtCore/QTextStream>
#include <QtCore/QPair>
#include <QtCore/QDebug>

#include <Common/Global.h>

#define IMPLEMENTATION NEW // OLD or NEW

#define NEW 1
#define OLD 2

#if IMPLEMENTATION == OLD
   #include <OldWordIndex.h>
   using namespace Old;
#else
   #include <Core/FileManager/priv/WordIndex/WordIndex.h>
   using namespace FM;
#endif

QTextStream in(stdin);
QTextStream out(stdout);

template <typename T>
void add(WordIndex<T>& index, const QString& word, const T& item)
{
   out << "Add item : " << item << " indexed by " << word << endl;
   index.addItem(word, item);
}

/**
  * Repeated n times.
  */
template <typename T>
void search(WordIndex<T>& index, const QString& word, int n)
{
   QElapsedTimer t;
   t.start();

   for (int i = 1; i < n; ++i)
      index.search(word);

   QList<T> items = WordIndex<T>::resultToList(index.search(word));

   qint64 time = t.elapsed();

   out << "Search \"" << word << "\" : " << items.count() << " items found" << endl;
   foreach (T item, items)
      out << " - " << item << endl;
   out << " Time: " << double(time) / 1000 << " s" << endl;
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
void indexFile(WordIndex<T>& index, const QString& fileName, const T& item)
{
   const QStringList& words = Common::Global::splitInWords(fileName);

   index.addItem(words, item);
}

template<typename T> T buildItem(const QFileInfo& entry);

// Full specialized function template.
template <>
int buildItem<int>(const QFileInfo& entry)
{
   static int n = 0;
   return ++n;
}
template <>
QString buildItem<QString>(const QFileInfo& entry)
{
   return QString().append(entry.absoluteFilePath()).append('/').append(entry.fileName());
}

template <typename T>
void buildIndex(WordIndex<T>& index, const QString& path)
{
   QLinkedList< QPair< QString, T > > items;

   out << "Scanning..." << endl;

   QLinkedList<QDir> dirsToVisit;
   dirsToVisit.append(path);
   while (!dirsToVisit.isEmpty())
   {
      foreach (QFileInfo entry, dirsToVisit.takeFirst().entryInfoList())
      {
         if (entry.fileName() == "." || entry.fileName() == "..")
            continue;

         items << qMakePair(entry.fileName(), buildItem<T>(entry)); // entry.absoluteFilePath(), entry.fileName()

         if (entry.isDir())
            dirsToVisit.append(entry.absoluteFilePath());
      }
   }

   out << "Indexing..." << endl;
   QElapsedTimer t;
   t.start();

   int n = 0;
   for (auto i = items.begin(); i != items.end(); ++i)
   {
      n += 1;
      indexFile(index, i->first, i->second);
   }

   out << n << " items indexed in " << double(t.elapsed()) / 1000 << " s" << endl;
}

void printUsage(int argc, char *argv[])
{
   QTextStream out(stdout);
   out << "Usage : " << argv[0] << " <directory>*" << endl
      << " <directory> : will scan recursively the directory and index each file and folder." << endl;
}

int main(int argc, char *argv[])
{
   if (argc >= 2)
   {
      // WordIndex<QString> index; // If a word index of string is used the item corresponds to fullpath + filename.
      WordIndex<int> index;

      for (int i = 1; i < argc; i++)
         buildIndex(index, argv[i]);

      forever
      {
         out << "Type a word : ";
         out.flush();
         const QString& itemToSearch = in.readLine();
         if (itemToSearch == "quit")
            return 0;
         search(index, itemToSearch, 20000);
      }
   }
   else
      printUsage(argc, argv);
}
