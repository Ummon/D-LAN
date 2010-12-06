#ifndef COMMON_COMMON_H
#define COMMON_COMMON_H

#include <exception>

#include <QtGlobal>
#include <QByteArray>

namespace Common
{
   class Global
   {
   public:
      class UnableToSetTempDirException : public std::exception
      {
      public:
         UnableToSetTempDirException(const QString& dir);
         ~UnableToSetTempDirException() throw() {};
         const char* what() const throw();
      private:
         QByteArray errorMessage;
      };

      static int nCombinations(int n, int k);
      static QString formatByteSize(qint64 bytes);
      static qint64 availableDiskSpace(const QString& path);
      static bool rename(const QString& existingFile, const QString& newFile);
      static bool createApplicationFolder();
      static void createFile(const QString& path);

      static bool recursiveDeleteDirectoryContent(const QString& dir);
      static bool recursiveDeleteDirectory(const QString& dir);
      static QString setCurrentDirToTemp(const QString& dir);
   };
}

#endif
