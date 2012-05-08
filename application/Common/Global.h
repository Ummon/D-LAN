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
  
#ifndef COMMON_COMMON_H
#define COMMON_COMMON_H

#include <functional>

#include <QObject>
#include <QString>
#include <QList>
#include <QMutableListIterator>

class QHostAddress;

namespace Common
{
   class Global
   {
   public:
      class UnableToSetTempDirException
      {
      public:
         UnableToSetTempDirException(const QString& dir);
         const QString errorMessage;
      };

      static QString getVersion();
      static QString getVersionTag();
      static QString getSystemVersion();
      static QString getVersionFull();

      static inline int commonPrefix(const QString& s1, const QString& s2) { return Global::commonPrefix(&s1, &s2); }
      static inline int commonPrefix(const QStringRef& s1, const QStringRef& s2);

      static int nCombinations(int n, int k);
      static QString formatByteSize(qint64 bytes, int precision = 1);
      static QString formatTime(quint64 seconds);
      static QString formatIP(const QHostAddress& address, quint16 port);
      static qint64 availableDiskSpace(const QString& path);
      static bool rename(const QString& existingFile, const QString& newFile);

      static bool isLocal(const QHostAddress& address);

      static QString cleanDirPath(const QString& path);
      static bool isWindowsPath(const QString& path);
      static bool isWindowsRootPath(const QString& path);

      static QString toLowerAndRemoveAccents(const QString& str);
      static QStringList splitInWords(const QString& words);
      static int strcmpi(const std::string& s1, const std::string& s2);

      static quint32 hashStringToInt(const QString& str);

      enum class DataFolderType { ROAMING = 0, LOCAL = 1 };

   private:
      static QString dataFolders[2];

   public:
      class UnableToGetFolder
      {
      public:
         UnableToGetFolder(const QString& message) : errorMessage(message) {}
         const QString errorMessage;
      };

      static QString getDataFolder(DataFolderType type, bool create = true);
      static void setDataFolder(DataFolderType type, const QString& folder);
      static void setDataFolderToDefault(DataFolderType type);

      static QString getDataServiceFolder(DataFolderType type);
      static QString getDataSystemFolder(DataFolderType type);

      static QString getCurrenUserName();
      static QString getCurrenMachineName();

      static bool createFile(const QString& path);
      static bool recursiveDeleteDirectoryContent(const QString& dir);
      static bool recursiveDeleteDirectory(const QString& dir);
      static QString setCurrentDirToTemp(const QString& dir);      

      static QString getQObjectHierarchy(const QObject* root);
      static QString getQObjectHierarchy(const QObject* root, std::function<QString(const QObject*)> fun);
   };
}

inline int Common::Global::commonPrefix(const QStringRef& s1, const QStringRef& s2)
{
   int i = 0;
   while (i < s1.size() && i < s2.size())
   {
      if (s1.at(i) != s2.at(i))
         return i;
      ++i;
   }
   return i;
}

#endif
