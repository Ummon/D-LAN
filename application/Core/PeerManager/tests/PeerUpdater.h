#ifndef TESTS_PEERMANAGER_PEERUPDATER_H
#define TESTS_PEERMANAGER_PEERUPDATER_H

#include <QSharedPointer>
#include <QTimer>
#include <QList>

#include <Core/FileManager/IFileManager.h>

#include <IPeerManager.h>
#include <IPeer.h>
using namespace PM;

struct PeerData
{
   Common::Hash ID;
   QHostAddress IP;
   int port;
   QString nick;
   quint64 sharingAmount;
};

class PeerUpdater : public QObject
{
   Q_OBJECT
public:
   PeerUpdater(QList< QSharedPointer<FM::IFileManager> > fileManagers, QList< QSharedPointer<IPeerManager> > peerManagers);

   void start();
   void stop();

private slots:
   void update();

private:
   QList< QSharedPointer<FM::IFileManager> > fileManagers;
   QList< QSharedPointer<IPeerManager> > peerManagers;
   QTimer timer;
};

#endif
