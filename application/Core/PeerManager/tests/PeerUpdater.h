#ifndef TESTS_PEERMANAGER_PEERUPDATER_H
#define TESTS_PEERMANAGER_PEERUPDATER_H

#include <QSharedPointer>
#include <QTimer>
#include <QList>

#include <IPeerManager.h>
#include <IPeer.h>
using namespace PM;

struct PeerData
{
   Common::Hash ID;
   QHostAddress IP;
   QString nick;
   quint64 sharingAmount;
};

class PeerUpdater : public QObject
{
   Q_OBJECT
public:
   PeerUpdater(QSharedPointer<IPeerManager> peerManager, int n);
   QList<PeerData> getPeers();
   void stop();

private slots:
   void update();

private:
   QSharedPointer<IPeerManager> peerManager;
   QList<PeerData> peers;
   QTimer timer;
};

#endif
