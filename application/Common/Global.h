/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef COMMON_COMMON_H
#define COMMON_COMMON_H

#include <QString>
#include <QList>
#include <QMutableListIterator>

namespace Common
{
   class Global
   {
   public:
      /**
        * Exception which can give a message shoulg inherit from this class.
        */
      class MessageException
      {
      public:
         virtual QString getMessage() const throw() = 0;
      };

      class UnableToSetTempDirException : public MessageException
      {
      public:
         UnableToSetTempDirException(const QString& dir);
         ~UnableToSetTempDirException() throw() {};
         QString getMessage() const throw();

      private:
         QString errorMessage;
      };

      template <typename T>
      static void sortedAdd(T* entry, QList<T*>& list);
      template <typename T>
      static void sortedAdd(const QList<T*>& entries, QList<T*>& list);

      static int nCombinations(int n, int k);
      static QString formatByteSize(qint64 bytes, int precision = 1);
      static qint64 availableDiskSpace(const QString& path);
      static bool rename(const QString& existingFile, const QString& newFile);
      static QString cleanDirPath(const QString& path);

      enum DataFolderType { ROAMING = 0, LOCAL = 1 };

   private:
      static QString dataFolders[2];

   public:
      class UnableToGetFolder {};
      static QString getDataFolder(DataFolderType type, bool create = true);
      static void setDataFolder(DataFolderType type, const QString& folder);
      static void setDataFolderToDefault(DataFolderType type);

      static QString getDataSystemFolder(DataFolderType type);

      static QString getCurrenUserName();
      static QString getCurrenMachineName();

      static bool createFile(const QString& path);
      static bool recursiveDeleteDirectoryContent(const QString& dir);
      static bool recursiveDeleteDirectory(const QString& dir);
      static QString setCurrentDirToTemp(const QString& dir);
   };
}

using namespace Common;

/**
  * Add an item into a sorted list. The list is kept sorted.
  * T must implement the < operator.
  * @param list Must be sorted.
  */
template <typename T>
void Global::sortedAdd(T* item, QList<T*>& list)
{
   for (QMutableListIterator<T*> i(list); i.hasNext(); i.next())
   {
      T* e = i.peekNext();
      if (e == item)
         return;
      if (*item < *e)
      {
         i.insert(item);
         return;
      }
   }

   list << item;
}

/**
  * Merge some items into a sorted list. The list is kept sorted.
  * T must implement the < operator.
  * @param list Must be sorted.
  */
template <typename T>
void Global::sortedAdd(const QList<T*>& items, QList<T*>& list)
{
   QListIterator<T*> i(items);
   QMutableListIterator<T*> j(list);

   while(i.hasNext())
   {
      T* ei = i.next();

      bool inserted = false;
      while (j.hasNext())
      {
         T* ej = j.peekNext();
         if (*ei < *ej)
         {
            j.insert(ei);
            i.next();
            inserted = true;
            break;
         }
         j.next();
      }

      if (!inserted)
         j.insert(ei);
   }
}

#endif
