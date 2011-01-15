/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef GUI_PEERLISTMODEL_H
#define GUI_PEERLISTMODEL_H

#include <QAbstractTableModel>

#include <Protos/gui_protocol.pb.h>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class PeerListModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      PeerListModel(QSharedPointer<RCC::ICoreConnection> coreConnection);
      QString getNick(const Common::Hash& peerID);
      bool isOurself(int rowNum) const;
      Common::Hash getPeerID(int rowNum) const;
      void clear();

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   signals:
      /**
        * To remove peer files browse window. (Not used).
        */
      void peersRemoved(QList<Common::Hash> peerIDs);

   private slots:
      void newState(const Protos::GUI::State& state);

   private:
      void setPeers(const google::protobuf::RepeatedPtrField<Protos::GUI::State_Peer>& peers);
      void sort();

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      struct Peer
      {
         Peer(const Common::Hash& peerID, const QString& nick, quint64 sharingAmount)
            : peerID(peerID), nick(nick), sharingAmount(sharingAmount) {}

         bool operator==(const Peer& p) const { return this->peerID == p.peerID; }
         bool operator!=(const Peer& p) const { return this->peerID != p.peerID; }
         bool operator<(const Peer& p) const { return this->sharingAmount < p.sharingAmount; }
         bool operator>(const Peer& p) const { return this->sharingAmount > p.sharingAmount; }

         Common::Hash peerID;
         QString nick;
         quint64 sharingAmount;
      };
      QList<Peer> peers;
   };
}

#endif
