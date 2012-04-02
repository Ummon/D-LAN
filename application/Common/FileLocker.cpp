#include <Common/FileLocker.h>
using namespace Common;

FileLocker::FileLocker(const QString& filePath, LockType type)
{

}

FileLocker::~FileLocker()
{

}

bool FileLocker::isLocked() const
{
   return true;
}
