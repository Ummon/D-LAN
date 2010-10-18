#ifndef FILEMANAGER_EXCEPTIONS_H
#define FILEMANAGER_EXCEPTIONS_H

#include <QStringList>

namespace FM
{
   class FileSystemEntriesNotFoundException
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
   class SuperDirectoryExistsException
   {
   public:
      SuperDirectoryExistsException(const QString& super, const QString& sub)
         : superDirectory(super), subDirectory(sub) {}
      virtual ~SuperDirectoryExistsException() throw() {}
      const QString superDirectory;
      const QString subDirectory;
   };

   // TODO : add some additionnals informations
   class SubDirectoriesWithDifferentRightsExistsException
   {
   public:
      SubDirectoriesWithDifferentRightsExistsException(const QString& super, const QStringList& subs)
         : superDirectory(super), subDirectories(subs) {}
      virtual ~SubDirectoriesWithDifferentRightsExistsException() throw() {}
      const QString superDirectory;
      const QStringList subDirectories;
   };

   class IOErrorException {};

   class UnableToOpenFileInWriteModeException {};

   class UnableToOpenFileInReadModeException {};

   class TryToWriteBeyondTheEndOfChunkException {};

   class DirAlreadySharedException {};

   class ChunkDeletedException {};

   class ChunkNotCompletedException {};

   class NoReadWriteSharedDirectoryException {};

   class InsufficientStorageSpaceException {};

   class UnableToCreateNewFileException {};

   class FilePhysicallyAlreadyExistsException {};
}

#endif
