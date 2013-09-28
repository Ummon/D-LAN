#ifndef GUI_PEERSDOCK_H
#define GUI_PEERSDOCK_H

#include <QDockWidget>
#include <QSharedPointer>
#include <QHostAddress>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Peers/PeerListModel.h>
#include <Peers/PeerListDelegate.h>

Q_DECLARE_METATYPE(QHostAddress)

namespace Ui {
   class PeersDock;
}

namespace GUI
{
   class PeersDock : public QDockWidget
   {
      Q_OBJECT

   public:
      explicit PeersDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = 0);
      ~PeersDock();

      PeerListModel& getModel();

   signals:
      void browsePeer(const Common::Hash& peerID);

   private slots:
      void displayContextMenuPeers(const QPoint& point);

      void browse();
      void takeControlOfACore();
      void copyIPToClipboard();

      void sortPeersBySharingAmount();
      void sortPeersByNick();
      void colorizeSelectedPeer();
      void uncolorizeSelectedPeer();

      void coreConnected();
      void coreDisconnected(bool force);

   private:
      void restoreColorizedPeers();

      Ui::PeersDock* ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      PeerListModel peerListModel;
      PeerListDelegate peerListDelegate;
   };
}

#endif
