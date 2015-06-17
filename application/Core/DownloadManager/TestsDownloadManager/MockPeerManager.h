#ifndef TESTS_DOWNLOADMANAGER_MOCKPEERMANAGER_H
#define TESTS_DOWNLOADMANAGER_MOCKPEERMANAGER_H

#include <PeerManager/IPeerManager.h>

class MockPeerManager : public PM::IPeerManager
{
   Q_OBJECT
public:
   MockPeerManager();
   ~MockPeerManager();

   void setNick(const QString& nick);
   PM::IPeer* getSelf();
   int getNbOfPeers() const;
   QList<PM::IPeer*> getPeers() const;
   PM::IPeer* getPeer(const Common::Hash& ID);
   PM::IPeer* createPeer(const Common::Hash& ID, const QString& nick);
   void updatePeer(
      const Common::Hash& ID,
      const QHostAddress& IP,
      quint16 port,
      const QString& nick,
      const quint64& sharingAmount,
      const QString& coreVersion,
      quint32 downloadRate,
      quint32 uploadRate,
      quint32 protocolVersion
   );
   void removePeer(const Common::Hash& ID, const QHostAddress& IP);
   void removeAllPeers();
   void newConnection(QTcpSocket* tcpSocket);

private:
   int createPeerNbCall;
};

#endif
