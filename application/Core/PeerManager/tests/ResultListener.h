#ifndef TESTS_PEERMANAGER_RESULTLISTENER_H
#define TESTS_PEERMANAGER_RESULTLISTENER_H

#include <QObject>
#include <QSharedPointer>

#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

#include <ISocket.h>
using namespace PM;

class ResultListener : public QObject
{
   Q_OBJECT
public:
   ResultListener();

   QList<Protos::Common::Entries> getEntriesResultList() const;
   int getNbEntriesResultReceived() const;

public slots:
   void entriesResult(const Protos::Common::Entries& entries);

   void result(const Protos::Core::GetHashesResult& result);
   void nextHash(const Common::Hash& hash);

   void result(const Protos::Core::GetChunkResult& result);
   void stream(QSharedPointer<ISocket> socket);
   void getChunk(Common::Hash hash, int offset, QSharedPointer<PM::ISocket> socket);

private:
   QList<Protos::Common::Entries> entriesResultList;
   int nbHashes;
   int currentHash;
};

#endif
