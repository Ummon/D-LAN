#ifndef TESTS_PEERMANAGER_RESULTLISTENER_H
#define TESTS_PEERMANAGER_RESULTLISTENER_H

#include <QObject>

#include <Protos/core_protocol.pb.h>

class ResultListener : public QObject
{
   Q_OBJECT
public slots:
   void entriesResult(const Protos::Core::GetEntriesResult& entries);
};

#endif
