#ifndef TESTS_FILEMANAGER_HASHESRECEIVER_H
#define TESTS_FILEMANAGER_HASHESRECEIVER_H

#include <QObject>
#include <QList>

#include <Common/Hash.h>

class HashesReceiver : public QObject
{
   Q_OBJECT
public:
   HashesReceiver();
   bool waitToReceive(QList<Common::Hash>& hashes, int timeout);

public slots:
   void nextHash(Common::Hash hash);

private:
   QList<Common::Hash> receivedHashes;
   int num;
};

#endif
