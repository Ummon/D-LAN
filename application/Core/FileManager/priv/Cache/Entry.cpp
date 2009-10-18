#include "Entry.h"
using namespace FileManager;

#include <priv/Cache/SharedDirectory.h>
#include <priv/FileManager.h>

Entry::Entry(const QString& name)
   : name(name), size(0)
{
}

QString Entry::getName()
{
   return this->name;
}

qint64 Entry::getSize()
{
   return this->size;
}
