/**
  * Aybabtu - A decentralized LAN file sharing software.
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

namespace Common
{
   class Global
   {
      static const QString APPLICATION_FOLDER_NAME;

   public:
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

      static int nCombinations(int n, int k);
      static QString formatByteSize(qint64 bytes, int precision = 1);
      static qint64 availableDiskSpace(const QString& path);
      static bool rename(const QString& existingFile, const QString& newFile);

      enum DataFolderType { ROAMING, LOCAL };
      class UnableToGetFolder {};
      static QString getDataFolder(DataFolderType type, bool create = true);

      static QString getCurrenUserName();
      static QString getCurrenMachineName();

      static bool createFile(const QString& path);
      static bool recursiveDeleteDirectoryContent(const QString& dir);
      static bool recursiveDeleteDirectory(const QString& dir);
      static QString setCurrentDirToTemp(const QString& dir);
   };
}

#endif
