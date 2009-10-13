#ifndef TESTS_H
#define TESTS_H

#include <QObject>
#include <QSharedPointer>

#include <INetworkListener.h>

using namespace NetworkListener;

class Tests : public QObject
{
   Q_OBJECT
public:
   Tests();

private slots:
   void initTestCase();


private :

};

#endif
