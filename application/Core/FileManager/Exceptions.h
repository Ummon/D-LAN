#ifndef FILEMANAGER_EXCEPTIONS_H
#define FILEMANAGER_EXCEPTIONS_H

#include <exception>
using namespace std;

#include <QStringList>

namespace FM
{
   class FileSystemEntriesNotFoundException : public exception
   {
   public :
      FileSystemEntriesNotFoundException(const QStringList& paths) : paths(paths) {}
      virtual ~FileSystemEntriesNotFoundException() throw () {}
      const QStringList paths;
   };

   class FilesNotFoundException : public FileSystemEntriesNotFoundException
   {
   public :
      FilesNotFoundException(const QStringList& paths) : FileSystemEntriesNotFoundException(paths) {}
      virtual ~FilesNotFoundException() throw () {}
   };

   class DirsNotFoundException : public FileSystemEntriesNotFoundException
   {
   public :
      DirsNotFoundException(const QStringList& paths) : FileSystemEntriesNotFoundException(paths) {}
      virtual ~DirsNotFoundException() throw () {}
   };

   /**
     * Thrown when adding a shared directory when a super directory is already shared.
     */
   class SuperDirectoryExistsException : public exception
   {
   public:
      SuperDirectoryExistsException(const QString& directory) : directory(directory) {}
      virtual ~SuperDirectoryExistsException() throw() {}
      const QString directory;
   };

   // TODO : add some additionnals informations
   class SubDirectoriesWithDifferentRightsExistsException : public exception
   {
   public:
      virtual ~SubDirectoriesWithDifferentRightsExistsException() throw() {}
   };

   class DirAlreadySharedException: public exception
   {
   };
}

#endif
