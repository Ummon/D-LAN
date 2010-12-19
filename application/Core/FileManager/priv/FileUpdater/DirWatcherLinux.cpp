#include "DirWatcherLinux.h"
using namespace FM;

#include <QMutexLocker>

DirWatcherLinux::DirWatcherLinux()
   : mutex(QMutex::Recursive)
{
}

DirWatcherLinux::~DirWatcherWin()
{
   QMutexLocker locker(&this->mutex);
}

bool DirWatcherLinux::addDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);
}

void DirWatcherLinux::rmDir(const QString& path)
{
   QMutexLocker locker(&this->mutex);
}

int DirWatcherLinux::nbWatchedDir()
{
   QMutexLocker locker(&this->mutex);
}

const QList<WatcherEvent> DirWatcherLinux::waitEvent(QList<WaitCondition*> ws)
{
}

const QList<WatcherEvent> DirWatcherLinux::waitEvent(int timeout, QList<WaitCondition*> ws)
{
   this->mutex.lock();

   this->mutex.unlock();

   QMutexLocker locker(&this->mutex);

   return QList<WatcherEvent>();
}
