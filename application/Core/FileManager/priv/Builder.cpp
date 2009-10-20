#include <Builder.h>
using namespace FM;

#include <string>

#include <IFileManager.h>
#include <priv/FileManager.h>

QSharedPointer<IFileManager> Builder::newFileManager()
{
   return QSharedPointer<IFileManager>(new FileManager());
}
