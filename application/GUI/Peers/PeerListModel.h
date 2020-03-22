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
  
#pragma once

#include <QAbstractTableModel>
#include <QHostAddress>
#include <QList>
#include <QHash>
#include <QColor>

#include <Protos/gui_protocol.pb.h>
#include <Protos/gui_settings.pb.h>

#include <Common/Hash.h>
#include <Common/Containers/SortedArray.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class PeerListModel : public QAbstractTableModel
   {
      Q_OBJECT
      class Peer;

   public:
      PeerListModel(QSharedPointer<RCC::ICoreConnection> coreConnection);
      ~PeerListModel();

      QString getNick(const Common::Hash& peerID, const QString& defaultNick = QString()) const;      
      QString getNick(int rowNum) const;
      bool isOurself(int rowNum) const;
      Common::Hash getPeerID(int rowNum) const;
      QHostAddress getPeerIP(int rowNum) const;
      Protos::GUI::State::Peer::PeerStatus getStatus(int rowNum) const;

      void setSortType(Protos::GUI::Settings::PeerSortType sortType);
      Protos::GUI::Settings::PeerSortType getSortType() const;
      void setDisplayOnlyPeersWithStatusOK(bool displayed);
      void setToolTipEnabled(bool enabled);
      void setRoom(const QString& room);
      void rmRoom();

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

      void colorize(const Common::Hash& peerID, const QColor& color);
      void colorize(const QModelIndex& index, const QColor& color);
      void uncolorize(const QModelIndex& index);

      /**
        * May be encapsulated in a 'QVariant' returned by 'data(..)'.
        */
      struct TransferInformation
      {
         quint32 downloadRate;
         quint32 uploadRate;
         bool isDownloadingOurData;

         bool operator==(const TransferInformation& ti) const { return ti.downloadRate == this->downloadRate && ti.uploadRate == this->uploadRate && ti.isDownloadingOurData == this->isDownloadingOurData; }
         bool operator!=(const TransferInformation& ti) const { return !(ti == *this); }
      };

   signals:
      /**
        * To remove peer files browse window. (Not used).
        */
      void peersRemoved(QList<Common::Hash> peerIDs);

   private slots:
      void newState(const Protos::GUI::State& state);
      void coreDisconnected(bool forced);

   private:
      void updatePeers(const google::protobuf::RepeatedPtrField<Protos::GUI::State::Peer>& peers, const QSet<Common::Hash>& peersDownloadingOurData = QSet<Common::Hash>(), const QSet<Common::Hash>& peersToDisplay = QSet<Common::Hash>());

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      Common::SortedArray<Peer*> orderedPeers;
      QHash<Common::Hash, Peer*> indexedPeers; // Peers indexed by their ID.
      QHash<Common::Hash, QColor> peersToColorize;
      Protos::GUI::Settings::PeerSortType currentSortType;

      bool displayOnlyPeersWithStatusOK;
      bool toolTipEnabled;
      QString room;
   };
}

Q_DECLARE_METATYPE(GUI::PeerListModel::TransferInformation)
