#include <priv/SharedDirectory.h>
using namespace FileManager;


SharedDirectory::SharedDirectory(const QString& path)
   : Directory(path)
{
}

QString SharedDirectory::getPath()
{
   return this->name;
}
