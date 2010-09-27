#ifndef TESTS_PEERMANAGER_RESULTLISTENER_H
#define TESTS_PEERMANAGER_RESULTLISTENER_H

#include <QObject>

#include <Protos/core_protocol.pb.h>

#include <Common/Hash.h>

class ResultListener : public QObject
{
   Q_OBJECT
public:
   ResultListener();

   QList<Protos::Core::GetEntriesResult> getEntriesResultList() const;
   int getNbEntriesResultReceived() const;

public slots:
   void entriesResult(const Protos::Core::GetEntriesResult& entries);

   void result(const Protos::Core::GetHashesResult& result);
   void nextHash(const Common::Hash& hash);

private:
   QList<Protos::Core::GetEntriesResult> entriesResultList;
   int nbHashes;
   int currentHash;
};

#endif
