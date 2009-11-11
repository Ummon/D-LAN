#ifndef FILEMANAGER_PRIV_EXCEPTIONS_H
#define FILEMANAGER_PRIV_EXCEPTIONS_H

#include <exception>
using namespace std;

#include <QString>

namespace FM
{
   class FileSystemEntryNotFoundException : public exception
   {
   public :
      FileSystemEntryNotFoundException(const QString& path) : path(path) {}
      virtual ~FileSystemEntryNotFoundException() throw () {}
      const QString path;
   };

   class FileNotFoundException : public FileSystemEntryNotFoundException
   {
   public :
      FileNotFoundException(const QString& path) : FileSystemEntryNotFoundException(path) {}
      virtual ~FileNotFoundException() throw () {}
   };

   class DirNotFoundException : public FileSystemEntryNotFoundException
   {
   public :
      DirNotFoundException(const QString& path) : FileSystemEntryNotFoundException(path) {}
      virtual ~DirNotFoundException() throw () {}
   };

   /**
     * Throwed when an entry (file or directory) already exist
     * in its directory.
     */
   class EntryAlreadyExists : public exception {};
}

#endif
