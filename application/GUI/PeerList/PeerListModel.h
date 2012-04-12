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
  
#ifndef GUI_PEERLISTMODEL_H
#define GUI_PEERLISTMODEL_H

#include <QAbstractTableModel>
#include <QHostAddress>
#include <QList>
#include <QHash>
#include <QColor>

#include <Protos/gui_protocol.pb.h>
#include <Protos/gui_settings.pb.h>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class PeerListModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      PeerListModel(QSharedPointer<RCC::ICoreConnection> coreConnection);
      ~PeerListModel();

      QString getNick(const Common::Hash& peerID, const QString& defaultNick = QString()) const;
      bool isOurself(int rowNum) const;
      Common::Hash getPeerID(int rowNum) const;
      QHostAddress getPeerIP(int rowNum) const;
      void clear();

      void setSortType(Protos::GUI::Settings::PeerSortType sortType);
      Protos::GUI::Settings::PeerSortType getSortType() const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

      void colorize(const Common::Hash& peerID, const QColor& color);
      void colorize(const QModelIndex& index, const QColor& color);
      void uncolorize(const QModelIndex& index);

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
         Peer(const Common::Hash& peerID, const QString& nick, const QString& coreVersion, quint64 sharingAmount, const QHostAddress& ip) :
            peerID(peerID), nick(nick), coreVersion(coreVersion), sharingAmount(sharingAmount), ip(ip) {}

         bool operator==(const Peer& p) const { return this->peerID == p.peerID; }
         bool operator!=(const Peer& p) const { return this->peerID != p.peerID; }
         static bool sortCompByNick(const Peer* p1, const Peer* p2);
         static bool sortCompBySharingAmount(const Peer* p1, const Peer* p2);

         Common::Hash peerID;
         QString nick;
         QString coreVersion;
         quint64 sharingAmount;
         QHostAddress ip;
      };


      QList<Peer*> peers;
      QHash<Common::Hash, Peer*> indexedPeers; // Peers indexed by their ID.
      QHash<Common::Hash, QColor> peersToColorize;
      Protos::GUI::Settings::PeerSortType currentSortType;
   };
}

#endif
