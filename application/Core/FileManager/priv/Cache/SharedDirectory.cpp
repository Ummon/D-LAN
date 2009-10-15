#include <priv/Cache/SharedDirectory.h>
using namespace FileManager;

SharedDirectory::SharedDirectory(FileManager* fileManager, const QString& path)
   : Directory(path), fileManager(fileManager)
{
}

QString SharedDirectory::getPath()
{
   return this->name;
}

FileManager::FileManager* SharedDirectory::getFileManager()
{
   return this->fileManager;
}
