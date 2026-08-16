#include <MockFileManager.h>

MockFileManager::MockFileManager()
{

}

MockFileManager::~MockFileManager()
{

}

void MockFileManager::setSharedPaths(const QList<SharedPath>& paths)
{

}

QPair<Common::SharedEntry, QString> MockFileManager::addASharedPath(const QString& absoluteDir)
{
   return qMakePair(Common::SharedEntry(), QString());
}

QList<Common::SharedEntry> MockFileManager::getSharedEntries() const
{
   return QList<Common::SharedEntry>();
}

QString MockFileManager::getSharedEntry(const Common::Hash& ID) const
{
   return QString();
}

QSharedPointer<FM::IChunk> MockFileManager::getChunk(const Common::Hash& hash) const
{
   return QSharedPointer<FM::IChunk>();
}

QList<QSharedPointer<FM::IChunk>> MockFileManager::getAllChunks(const Protos::Common::Entry& localEntry, const QList<Common::Hash>& hashes) const
{
   return QList<QSharedPointer<FM::IChunk>>();
}

void MockFileManager::updateFromQueueEntry(const Protos::Queue::Queue_Entry& entry)
{
}

QList<QSharedPointer<FM::IChunk>> MockFileManager::newFile(Protos::Common::Entry& entry)
{
   return QList<QSharedPointer<FM::IChunk>>();
}

void MockFileManager::newDirectory(Protos::Common::Entry& entry)
{

}

QSharedPointer<FM::IGetHashesResult> MockFileManager::getHashes(const Protos::Common::Entry& file)
{
   return QSharedPointer<FM::IGetHashesResult>();
}

QSharedPointer<FM::IGetEntriesResult> MockFileManager::getScannedEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry)
{
   return QSharedPointer<FM::IGetEntriesResult>();
}

Protos::Common::Entries MockFileManager::getEntries(const Protos::Common::Entry& dir, int maxNbHashesPerEntry)
{
   return Protos::Common::Entries();
}

Protos::Common::Entries MockFileManager::getEntries()
{
   return Protos::Common::Entries();
}

QList<Protos::Common::FindResult> MockFileManager::find(const QString& words, int maxNbResult, int maxSize)
{
   return QList<Protos::Common::FindResult>();
}

QList<Protos::Common::FindResult> MockFileManager::find(const QString& words, const QList<QString>& extensions, qint64 minFileSize, qint64 maxFileSize, Protos::Common::FindPattern_Category category, int maxNbResult, int maxSize, bool setSharedEntryPath)
{
   return QList<Protos::Common::FindResult>();
}

QBitArray MockFileManager::haveChunks(const QList<Common::Hash>& hashes)
{
   return QBitArray();
}

qint64 MockFileManager::getAmount()
{
   return 0;
}

FM::IFileManager::CacheStatus MockFileManager::getCacheStatus() const
{
   return FM::IFileManager::CacheStatus::SCANNING_IN_PROGRESS;
}

int MockFileManager::getProgress() const
{
   return 0;
}

QString MockFileManager::getWordIndex_debug() const
{
   return QString();
}

QString MockFileManager::getSimilarFiles_debug() const
{
   return QString();
}

QString MockFileManager::getCacheTree_debug() const
{
   return QString();
}
