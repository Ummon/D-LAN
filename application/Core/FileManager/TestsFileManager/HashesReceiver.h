#ifndef TESTS_FILEMANAGER_HASHESRECEIVER_H
#define TESTS_FILEMANAGER_HASHESRECEIVER_H

#include <QObject>

#include <Common/Hash.h>

class HashesReceiver : public QObject
{
   Q_OBJECT
public:
   HashesReceiver();

public slots:
   void nextHash(Common::Hash hash);

private:
   int num;
};

#endif
