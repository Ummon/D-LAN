#include <SearchCoordinator.h>
using namespace StressTests;

#include <QMutexLocker>

#include <Paths.h>

SearchCoordinator::FileLock::FileLock(SearchCoordinator& coordinator, quint64 id) :
   coordinator(coordinator),
   id(id)
{
}

SearchCoordinator::FileLock::~FileLock()
{
   this->coordinator.unlock(this->id);
}

SearchCoordinator::SearchCoordinator(int numberOfCores) :
   cores(numberOfCores)
{
}

void SearchCoordinator::setCoreAvailable(int coreNumber, const Common::Hash& peerID, const QList<SharedEntry>& sharedEntries)
{
   QMutexLocker locker(&this->mutex);
   CoreInfo& info = this->cores[coreNumber];
   info.peerID = peerID;
   info.sharedEntries = sharedEntries;
   if (info.availableSince.isNull())
      info.availableSince = QDateTime::currentDateTimeUtc();
}

void SearchCoordinator::setCoreUnavailable(int coreNumber)
{
   QMutexLocker locker(&this->mutex);
   this->cores[coreNumber].availableSince = QDateTime();
}

bool SearchCoordinator::getCoreInfo(int coreNumber, CoreInfo& info) const
{
   QMutexLocker locker(&this->mutex);
   if (this->cores[coreNumber].availableSince.isNull())
      return false;
   info = this->cores[coreNumber];
   return true;
}

std::unique_ptr<SearchCoordinator::FileLock> SearchCoordinator::lockFile(int coreNumber, const QString& filepath)
{
   QMutexLocker locker(&this->mutex);

   if (this->cores[coreNumber].availableSince.isNull())
      return nullptr;

   for (const QString& path : this->beingDeleted)
      if (isSameOrInside(filepath, path))
         return nullptr;

   const quint64 id = this->nextLockID++;
   this->locks.insert(id, Lock { coreNumber, filepath });
   return std::unique_ptr<FileLock>(new FileLock(*this, id));
}

bool SearchCoordinator::beginDelete(const QString& path)
{
   QMutexLocker locker(&this->mutex);

   for (const Lock& lock : this->locks)
      if (isSameOrInside(lock.filepath, path))
         return false;

   this->beingDeleted << path;
   return true;
}

void SearchCoordinator::endDelete(const QString& path)
{
   QMutexLocker locker(&this->mutex);
   this->beingDeleted.removeOne(path);
}

bool SearchCoordinator::beginRestart(int coreNumber)
{
   QMutexLocker locker(&this->mutex);

   for (const Lock& lock : this->locks)
      if (lock.coreNumber == coreNumber)
         return false;

   this->cores[coreNumber].availableSince = QDateTime();
   return true;
}

void SearchCoordinator::unlock(quint64 id)
{
   QMutexLocker locker(&this->mutex);
   this->locks.remove(id);
}
