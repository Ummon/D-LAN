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
      SuperDirectoryExistsException(const QString& super, const QString& sub)
         : superDirectory(super), subDirectory(sub) {}
      virtual ~SuperDirectoryExistsException() throw() {}
      const QString superDirectory;
      const QString subDirectory;
   };

   // TODO : add some additionnals informations
   class SubDirectoriesWithDifferentRightsExistsException : public exception
   {
   public:
      SubDirectoriesWithDifferentRightsExistsException(const QString& super, const QStringList& subs)
         : superDirectory(super), subDirectories(subs) {}
      virtual ~SubDirectoriesWithDifferentRightsExistsException() throw() {}
      const QString superDirectory;
      const QStringList subDirectories;
   };

   class DirAlreadySharedException: public exception
   {
   };

   class ChunkDeletedException : public exception
   {
   };

   class NoReadWriteSharedDirectoryException : public exception
   {
   };

   class InsufficientStorageSpaceException : public exception
   {
   };
}

#endif
