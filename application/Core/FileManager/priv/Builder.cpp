#include <Builder.h>
using namespace FileManager;

#include <IFileManager.h>
#include <priv/FileManager.h>

QSharedPointer<IFileManager> Builder::newFileManager()
{
   return QSharedPointer<IFileManager>(new FileManager());
}
