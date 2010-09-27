#include <ResultListener.h>

#include <QtDebug>
#include <QString>

ResultListener::ResultListener()
   : nbHashes(0), currentHash(0)
{
}

QList<Protos::Core::GetEntriesResult> ResultListener::getEntriesResultList() const
{
   return this->entriesResultList;
}

int ResultListener::getNbEntriesResultReceived() const
{
   return this->entriesResultList.size();
}

void ResultListener::entriesResult(const Protos::Core::GetEntriesResult& entries)
{
   this->entriesResultList << entries;
   qDebug() << "ResultListener::entriesResult : " << QString::fromStdString(entries.DebugString());
}

void ResultListener::result(const Protos::Core::GetHashesResult& result)
{
   this->nbHashes = result.nb_hash();
   this->currentHash = 0;
   qDebug() << "ResultListener::result : " << QString::fromStdString(result.DebugString());
}

void ResultListener::nextHash(const Common::Hash& hash)
{
   qDebug() << "ResultListener::nextHash : [" << this->currentHash + 1 << "/" << this->nbHashes << "] " << hash.toStr();
   this->currentHash++;
}
