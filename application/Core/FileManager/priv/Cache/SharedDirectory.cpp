#include <priv/Cache/SharedDirectory.h>
using namespace FM;

#include <QDir>

#include <Exceptions.h>
#include <priv/Exceptions.h>
#include <priv/Cache/Cache.h>

SharedDirectory::SharedDirectory(Cache* cache, const QString& path, Rights rights)
   : Directory(cache), path(QDir::cleanPath(path)), rights(rights), id(Common::Hash::rand())
{
   this->init();
}

SharedDirectory::SharedDirectory(Cache* cache, const QString& path, Rights rights, const Common::Hash& id)
   : Directory(cache), path(QDir::cleanPath(path)), rights(rights), id(id)
{
   this->init();
}

void SharedDirectory::init()
{
   // Avoid two same directories.
   if (this->cache->isShared(this->path))
      throw DirAlreadySharedException();

   // First of all check is the directory physically exists.
   if (!QDir(this->path).exists())
      throw DirNotFoundException(this->path);

   if (this->cache->getSuperSharedDirectory(path))
      throw SuperDirectoryExistsException();

   // Gets the sub directories and checks the rights matches.
   QList<SharedDirectory*> subDirs = this->cache->getSubSharedDirectories(path);
   foreach (SharedDirectory* subDir, subDirs)
      if (subDir->rights != this->rights)
         throw SubDirectoriesWithDifferentRightsExistsException();

   // Merges the sub-directories of each directory found.
   foreach (SharedDirectory* subDir, subDirs)
   {
      // Create the missing directories.
      const QStringList& parentFolders = this->getFullPath().split('/', QString::SkipEmptyParts);
      const QStringList& childFolders = subDir->getFullPath().split('/', QString::SkipEmptyParts);
      Directory* current = this;
      for (int i = parentFolders.size(); i < childFolders.size(); i++)
         current = new Directory(this, childFolders[i]);
      current->stealSubDirs(subDir);

      delete subDir;
   }
}

SharedDirectory::~SharedDirectory()
{}

QList<File*> SharedDirectory::restoreFromFileCache(const Protos::FileCache::Hashes& hashes)
{
   QList<File*> ret;

   // Give each root to each sub directory. We don't match the full path.
   for (int i = 0; i < hashes.dir_size(); i++)
      ret << Directory::restoreFromFileCache(hashes.dir(i).root());

   return ret;
}

QString SharedDirectory::getPath() const
{
   return "";
}

QString SharedDirectory::getFullPath() const
{
   return this->path;
}

SharedDirectory::Rights SharedDirectory::getRights() const
{
   return this->rights;
}

const Common::Hash& SharedDirectory::getId() const
{
   return this->id;
}
