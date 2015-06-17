#include <MockFileManager.h>

MockFileManager::MockFileManager()
{

}

MockFileManager::~MockFileManager()
{

}

void MockFileManager::setSharedDirs(const QStringList& dirs)
{

}

QPair<Common::SharedDir, QString> MockFileManager::addASharedDir(const QString& absoluteDir)
{
   return qMakePair(Common::SharedDir(), QString());
}

QList<Common::SharedDir> MockFileManager::getSharedDirs() const
{
   return QList<Common::SharedDir>();
}

QString MockFileManager::getSharedDir(const Common::Hash& ID) const
{
   return QString();
}

QSharedPointer<FM::IChunk> MockFileManager::getChunk(const Common::Hash& hash) const
{
   return QSharedPointer<FM::IChunk>();
}

QList<QSharedPointer<FM::IChunk>> MockFileManager::getAllChunks(const Protos::Common::Entry& localEntry, const Common::Hashes& hashes) const
{
   return QList<QSharedPointer<FM::IChunk>>();
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

QSharedPointer<FM::IGetEntriesResult> MockFileManager::getScannedEntries(const Protos::Common::Entry& dir)
{
   return QSharedPointer<FM::IGetEntriesResult>();
}

Protos::Common::Entries MockFileManager::getEntries(const Protos::Common::Entry& dir)
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

QList<Protos::Common::FindResult> MockFileManager::find(const QString& words, const QList<QString>& extensions, qint64 minFileSize, qint64 maxFileSize, Protos::Common::FindPattern_Category category, int maxNbResult, int maxSize)
{
   return QList<Protos::Common::FindResult>();
}

QBitArray MockFileManager::haveChunks(const QList<Common::Hash>& hashes)
{
   return QBitArray();
}

quint64 MockFileManager::getAmount()
{
   return 0;
}

MockFileManager::CacheStatus MockFileManager::getCacheStatus() const
{
   return LOADING_CACHE_IN_PROGRSS;
}

int MockFileManager::getProgress() const
{
   return 0;
}

void MockFileManager::dumpWordIndex() const
{

}

void MockFileManager::printSimilarFiles() const
{

}
