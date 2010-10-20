#include <ResultListener.h>
using namespace PM;

#include <QtDebug>
#include <QTest>
#include <QString>

#include <Common/ProtoHelper.h>

#include <ISocket.h>

ResultListener::ResultListener()
   : nbHashes(0), currentHash(0)
{
}

QList<Protos::Common::Entries> ResultListener::getEntriesResultList() const
{
   return this->entriesResultList;
}

int ResultListener::getNbEntriesResultReceived() const
{
   return this->entriesResultList.size();
}

void ResultListener::entriesResult(const Protos::Common::Entries& entries)
{
   this->entriesResultList << entries;
   qDebug() << "ResultListener::entriesResult : " << Common::ProtoHelper::getDebugStr(entries);
}

void ResultListener::result(const Protos::Core::GetHashesResult& result)
{
   this->nbHashes = result.nb_hash();
   this->currentHash = 0;
   qDebug() << "ResultListener::result : " << Common::ProtoHelper::getDebugStr(result);
}

void ResultListener::nextHash(const Common::Hash& hash)
{
   qDebug() << "ResultListener::nextHash : [" << this->currentHash + 1 << "/" << this->nbHashes << "] " << hash.toStr();
   this->currentHash++;
}

void ResultListener::result(const Protos::Core::GetChunkResult& result)
{
   qDebug() << "ResultListener::result : " << Common::ProtoHelper::getDebugStr(result);
}

static const QByteArray CHUNK_DATA("HELLO");

void ResultListener::stream(ISocket* socket)
{
   QByteArray data = socket->getQSocket()->readAll();
   qDebug() << "ResultListener::stream : " << data;
   QCOMPARE(data, CHUNK_DATA);
   socket->finished();
}

void ResultListener::getChunk(Common::Hash hash, int offset, ISocket* socket)
{
   Protos::Core::GetChunkResult result;
   result.set_status(Protos::Core::GetChunkResult_Status_OK);
   socket->send(0x52, result);

   socket->getQSocket()->write(CHUNK_DATA);
   socket->finished();
}
