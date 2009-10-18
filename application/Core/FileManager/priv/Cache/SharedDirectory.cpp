#include <priv/Cache/SharedDirectory.h>
using namespace FileManager;

#include <priv/FileManager.h>

SharedDirectory::SharedDirectory(FileManager* fileManager, const QString& path)
   : Directory(path), fileManager(fileManager)
{
   this->id = Common::Hash::rand();

   // Same as a new file (see the File ctor).
   static_cast<SharedDirectory*>(this->getRoot())->getFileManager()->addToWordIndex(this);
}

QString SharedDirectory::getPath()
{
   return this->name;
}

FileManager::FileManager* SharedDirectory::getFileManager()
{
   return this->fileManager;
}

SharedDirectory::Rights SharedDirectory::getRights()
{
   return this->rights;
}
