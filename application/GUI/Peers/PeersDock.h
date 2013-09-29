/**
  * D-LAN - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2012 Greg Burri <greg.burri@gmail.com>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */
  
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
