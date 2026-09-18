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

#include <Peers/PeerListModel.h>
using namespace GUI;

#include <QGuiApplication>
#include <QPalette>
#include <QtAlgorithms>
#include <QStringBuilder>
#include <QSet>
#include <utility>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>
#include <Common/StringUtils.h>

#include <Log.h>

/**
  * @class PeerListModel
  *
  * The list of all peers. The list is built from the core state message, see the method 'newState(..)'.
  * The list can be order by the amount of sharing or in an alphabetic way, see the method 'setSortType(..)'.
  */

struct PeerListModel::Peer
{
   bool operator==(const Peer& p) const { return this->peerID == p.peerID; }
   bool operator!=(const Peer& p) const { return this->peerID != p.peerID; }

   Common::Hash peerID;
   QString nick;
   QString coreVersion;
   quint64 sharingAmount;
   QHostAddress ip;
   TransferInformation transferInformation;
   Protos::GUI::State::Peer::PeerStatus status;
};

const QColor PeerListModel::COLOR_OURSELF = QColor::fromHslF(0.17, 0.3, 0.0);
const QColor PeerListModel::COLOR_PEER_ERROR(160, 160, 160);
const QColor PeerListModel::COLOR_PEER_RED(118, 0, 0);
const QColor PeerListModel::COLOR_PEER_GREEN(0, 88, 0);
const QColor PeerListModel::COLOR_PEER_BLUE(0, 0, 108);

PeerListModel::PeerListModel(QSharedPointer<RCC::ICoreConnection> coreConnection) :
   coreConnection(coreConnection),
   orderedPeers([](const Peer* first, const Peer* second) {
      return peerLessThan(first, second, Protos::GUI::Settings::BY_SHARING_AMOUNT);
   }),
   currentSortType(Protos::GUI::Settings::BY_SHARING_AMOUNT),
   displayOnlyPeersWithStatusOK(false),
   toolTipEnabled(true)
{
   connect(this->coreConnection.data(), &RCC::ICoreConnection::newState, this, &PeerListModel::newState);
   connect(this->coreConnection.data(), &RCC::ICoreConnection::disconnected, this, &PeerListModel::coreDisconnected);
}

PeerListModel::~PeerListModel()
{
   auto end = this->orderedPeers.end();
   for (auto i = this->orderedPeers.begin(); i != end; ++i)
      delete *i;
}

/**
  * Return 'defaultNick' if the peer isn't found.
  */
QString PeerListModel::getNick(const Common::Hash& peerID, const QString& defaultNick) const
{
   Peer* peer = this->indexedPeers.value(peerID, 0);
   if (!peer)
      return defaultNick;
   return peer->nick;
}

QString PeerListModel::getNick(int rowNum) const
{
   if (rowNum < 0 || rowNum >= this->orderedPeers.size())
      return QString();
   return this->orderedPeers.getFromIndex(rowNum)->nick;
}

bool PeerListModel::isOurself(int rowNum) const
{
   if (rowNum < 0 || rowNum >= this->orderedPeers.size())
      return false;
   return this->orderedPeers.getFromIndex(rowNum)->peerID == this->coreConnection->getRemoteID();
}

Common::Hash PeerListModel::getPeerID(int rowNum) const
{
   if (rowNum < 0 || rowNum >= this->orderedPeers.size())
      return Common::Hash();
   return this->orderedPeers.getFromIndex(rowNum)->peerID;
}

QHostAddress PeerListModel::getPeerIP(int rowNum) const
{
   if (rowNum < 0 || rowNum >= this->orderedPeers.size())
      return QHostAddress();
   return this->orderedPeers.getFromIndex(rowNum)->ip;
}

Protos::GUI::State::Peer::PeerStatus PeerListModel::getStatus(int rowNum) const
{
   if (rowNum < 0 || rowNum >= this->orderedPeers.size())
      return Protos::GUI::State::Peer::OK;
   return this->orderedPeers.getFromIndex(rowNum)->status;
}

bool PeerListModel::peerLessThan(const Peer* first, const Peer* second, Protos::GUI::Settings::PeerSortType sortType)
{
   if (!first || !second)
      return false;
   if (sortType == Protos::GUI::Settings::BY_SHARING_AMOUNT && first->sharingAmount != second->sharingAmount)
      return first->sharingAmount > second->sharingAmount;
   const QString firstNick = Common::StringUtils::toLowerAndRemoveAccents(first->nick);
   const QString secondNick = Common::StringUtils::toLowerAndRemoveAccents(second->nick);
   if (firstNick != secondNick)
      return firstNick < secondNick;
   if (first->sharingAmount != second->sharingAmount)
      return first->sharingAmount > second->sharingAmount;
   return first->peerID < second->peerID;
}

// Find the insertion boundary without copying or modifying the sorted collection.
int PeerListModel::insertionPosition(Peer* peer) const
{
   const int previous = this->orderedPeers.indexOfNearest(peer);
   if (previous < 0)
      return 0;
   return previous + (peerLessThan(this->orderedPeers.getFromIndex(previous), peer, this->currentSortType) ? 1 : 0);
}

void PeerListModel::setSortType(Protos::GUI::Settings::PeerSortType sortType)
{
   if ((sortType != Protos::GUI::Settings::BY_NICK && sortType != Protos::GUI::Settings::BY_SHARING_AMOUNT) ||
       sortType == this->currentSortType)
      return;

   emit layoutAboutToBeChanged();
   // A view can create persistent indexes in response to the signal above.
   const QModelIndexList oldIndexes = this->persistentIndexList();
   QList<Peer*> indexed;
   for (const auto& index : oldIndexes)
      indexed.append(this->orderedPeers.getFromIndex(index.row()));

   this->currentSortType = sortType;
   this->orderedPeers.setSortedFunction([sortType](const Peer* first, const Peer* second) {
      return peerLessThan(first, second, sortType);
   });

   QModelIndexList newIndexes;
   for (int i = 0; i < oldIndexes.size(); ++i)
      newIndexes.append(this->index(this->orderedPeers.indexOf(indexed[i]), oldIndexes[i].column()));
   this->changePersistentIndexList(oldIndexes, newIndexes);
   emit layoutChanged();
}

Protos::GUI::Settings::PeerSortType PeerListModel::getSortType() const
{
   return this->currentSortType;
}

void PeerListModel::setDisplayOnlyPeersWithStatusOK(bool displayed)
{
   this->displayOnlyPeersWithStatusOK = displayed;
}

void PeerListModel::setToolTipEnabled(bool enabled)
{
   this->toolTipEnabled = enabled;
}

/**
  * To show only peer from a certain room.
  */
void PeerListModel::setRoom(const QString& room)
{
   this->room = room;
}

void PeerListModel::rmRoom()
{
   this->room = QString();
}

int PeerListModel::rowCount(const QModelIndex& parent) const
{
   return parent.isValid() ? 0 : this->orderedPeers.size();
}

int PeerListModel::columnCount(const QModelIndex& parent) const
{
   return parent.isValid() ? 0 : 3;
}

QVariant PeerListModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->orderedPeers.size())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      switch (index.column())
      {
      case 0: return QVariant::fromValue(this->orderedPeers.getFromIndex(index.row())->transferInformation);
      case 1: return this->orderedPeers.getFromIndex(index.row())->nick;
      case 2: return Common::Global::formatByteSize(this->orderedPeers.getFromIndex(index.row())->sharingAmount);
      default: return QVariant();
      }

   case Qt::BackgroundRole:
      if (this->peersToColorize.contains(this->orderedPeers.getFromIndex(index.row())->peerID))
         return this->peersToColorize[this->orderedPeers.getFromIndex(index.row())->peerID];
      if (this->isOurself(index.row()))
      {
         auto color = QGuiApplication::palette().color(QPalette::Normal, QPalette::Window).toHsl();
         color.setHslF(COLOR_OURSELF.hslHueF(), COLOR_OURSELF.hslSaturationF(), color.lightnessF());
         return color;
      }
      return QVariant();

   case Qt::ForegroundRole:
      if (this->orderedPeers.getFromIndex(index.row())->status != Protos::GUI::State::Peer::OK)
         return COLOR_PEER_ERROR;
      if (this->peersToColorize.contains(this->orderedPeers.getFromIndex(index.row())->peerID))
         return QColor(240, 240, 240);
      if (this->isOurself(index.row()))
      {
         auto color = QGuiApplication::palette().color(QPalette::Normal, QPalette::WindowText).toHsl();
         color.setHslF(COLOR_OURSELF.hslHueF(), COLOR_OURSELF.hslSaturationF(), color.lightnessF());
         return color;
      }
      return QVariant();

   case Qt::TextAlignmentRole:
      return QVariant((index.column() == 2 ? Qt::AlignRight : Qt::AlignLeft) | Qt::AlignVCenter);

   case Qt::ToolTipRole:
      if (this->toolTipEnabled)
      {
         const Peer* peer = this->orderedPeers.getFromIndex(index.row());
         const QString coreVersion = peer->coreVersion;
         QString toolTip = peer->nick;
         toolTip.append('\n');

         if (peer->status == Protos::GUI::State::Peer::MORE_RECENT_VERSION)
            toolTip.append(tr("Their protocol version is more recent and incompatible with ours. Upgrade you version!")).append('\n');
         else if (peer->status == Protos::GUI::State::Peer::VERSION_OUTDATED)
            toolTip.append(tr("Their protocol version is outdated and incompatible with ours. They should upgrade their version!")).append('\n');

         if (!coreVersion.isEmpty())
            toolTip += tr("Version %1\n").arg(coreVersion);
         toolTip +=
            tr("Download rate: ") % Common::Global::formatByteSize(peer->transferInformation.downloadRate) % "/s\n" %
            tr("Upload rate: ") % Common::Global::formatByteSize(peer->transferInformation.uploadRate) % "/s";
         return toolTip;
      }
      else
         return QVariant();

   default:
      return QVariant();
   }
}

void PeerListModel::colorize(const Common::Hash& peerID, const QColor& color)
{
   if (Peer* peer = this->indexedPeers.value(peerID, 0))
   {
      this->colorize(this->index(this->orderedPeers.indexOf(peer), 0), color);
   }
   else
      this->peersToColorize.insert(peerID, color);
}

void PeerListModel::colorize(const QModelIndex& index, const QColor& color)
{
   if (!index.isValid() || index.row() >= this->orderedPeers.size())
      return;

   this->peersToColorize[this->orderedPeers.getFromIndex(index.row())->peerID] = color;

   emit dataChanged(this->createIndex(index.row(), 0), this->createIndex(index.row(), this->columnCount() - 1));
}

void PeerListModel::uncolorize(const QModelIndex& index)
{
   if (!index.isValid() || index.row() >= this->orderedPeers.size())
      return;

   if (this->peersToColorize.remove(this->orderedPeers.getFromIndex(index.row())->peerID))
      emit dataChanged(this->createIndex(index.row(), 0), this->createIndex(index.row(), this->columnCount() - 1));
}

void PeerListModel::newState(const Protos::GUI::State& state)
{
   QSet<Common::Hash> peersDownloadingOurData;
   for (int i = 0; i < state.uploads_size(); i++)
      peersDownloadingOurData << Common::Hash(state.uploads(i).peer_id().hash());

   QSet<Common::Hash> peersToDisplay;
   if (!this->room.isEmpty())
      for (int i = 0; i < state.rooms_size(); i++)
         if (state.rooms(i).name() == this->room.toStdString())
         {
            for (int j = 0; j < state.rooms(i).peer_ids_size(); j++)
               peersToDisplay << Common::Hash(state.rooms(i).peer_ids(j).hash());

            if (state.rooms(i).joined())
               peersToDisplay << this->coreConnection->getRemoteID();

            break;
         }

   this->updatePeers(state.peers(), peersDownloadingOurData, peersToDisplay);
}

void PeerListModel::coreDisconnected(bool forced)
{
   google::protobuf::RepeatedPtrField<Protos::GUI::State_Peer> peers;
   this->updatePeers(peers);
}

/**
  * @param peersToDisplay If empty then all peers are displayed.
  */
void PeerListModel::updatePeers(
   const google::protobuf::RepeatedPtrField<Protos::GUI::State::Peer>& peers,
   const QSet<Common::Hash>& peersDownloadingOurData,
   const QSet<Common::Hash>& peersToDisplay
)
{
   QSet<Peer*> changedPeers;

   auto peersList = this->indexedPeers.keys();
   QSet<Common::Hash> peersToRemove(peersList.begin(), peersList.end());

   for (int i = 0; i < peers.size(); i++)
   {
      const Common::Hash peerID { peers.Get(i).peer_id().hash() };

      // We ignore some peers depending 'peersToDisplay' and 'this->displayOnlyPeersWithStatusOK'.
      if (
         (!peersToDisplay.isEmpty() && !peersToDisplay.contains(peerID)) ||
         (this->displayOnlyPeersWithStatusOK && peers.Get(i).status() != Protos::GUI::State::Peer::OK)
      )
         continue;

      const QString nick = QString::fromStdString(peers.Get(i).nick());
      const QString coreVersion = QString::fromStdString(peers.Get(i).core_version());
      const quint64 sharingAmount = peers.Get(i).sharing_amount();
      const TransferInformation transferInformation {
         peers.Get(i).download_rate(), peers.Get(i).upload_rate(),  peersDownloadingOurData.contains(peerID)
      };
      const Protos::GUI::State::Peer::PeerStatus status = peers.Get(i).status();
      const QHostAddress ip =
         peers.Get(i).has_ip() ?
            Common::ProtoHelper::getIP(peers.Get(i).ip()) :
            QHostAddress();

      auto peerIterator = this->indexedPeers.find(peerID);
      Peer* peer = peerIterator == this->indexedPeers.end() ? nullptr : *peerIterator;
      if (peer)
      {
         peersToRemove.remove(peerID);

         // 'nick' and 'sharingAmount' are the only fields the order depends on, see 'setSortType(..)'.
         const bool sortFieldsChanged = peer->nick != nick || peer->sharingAmount != sharingAmount;

         // Every field shown by 'data(..)', in a column or in the tool tip. Without 'status' and 'coreVersion'
         // here a peer going out of date keeps its normal colour and its old version until something else
         // changes. 'ip' is absent on purpose: it's never displayed, only read by 'getPeerIP(..)'.
         const bool displayChanged =
            sortFieldsChanged ||
            peer->transferInformation != transferInformation ||
            peer->status != status ||
            peer->coreVersion != coreVersion;

         if (displayChanged)
            changedPeers.insert(peer);

         if (sortFieldsChanged)
         {
            Peer updated = *peer;
            updated.nick = nick;
            updated.sharingAmount = sharingAmount;
            const int oldRow = this->orderedPeers.indexOf(peer);
            int newRow = this->insertionPosition(&updated);
            if (newRow > oldRow)
               --newRow; // The insertion boundary still includes the old entry.
            if (oldRow != newRow)
               this->beginMoveRows(QModelIndex(), oldRow, oldRow, QModelIndex(), newRow > oldRow ? newRow + 1 : newRow);

            // Remove before modifying the sort key so the entry can still be found.
            this->orderedPeers.remove(peer);
            peer->nick = nick;
            peer->sharingAmount = sharingAmount;
            this->orderedPeers.insert(peer);
            if (oldRow != newRow)
               this->endMoveRows();
         }

         peer->transferInformation = transferInformation;
         peer->status = status;
         peer->coreVersion = coreVersion;
         peer->ip = ip;
      }
      else
      {
         Peer* p = new Peer { peerID, nick, coreVersion, sharingAmount, ip, transferInformation, status };
         const int row = this->insertionPosition(p);
         this->beginInsertRows(QModelIndex(), row, row);
         this->indexedPeers.insert(peerID, p);
         this->orderedPeers.insert(p);
         this->endInsertRows();
      }
   }

   QList<Common::Hash> peerIDsRemoved;
   for (auto i = peersToRemove.begin(); i != peersToRemove.end(); ++i)
   {
      Peer* peer = this->indexedPeers[*i];
      const int row = this->orderedPeers.indexOf(peer);
      this->beginRemoveRows(QModelIndex(), row, row);
      peerIDsRemoved << peer->peerID;
      this->indexedPeers.remove(peer->peerID);
      this->orderedPeers.remove(peer);
      delete peer;
      this->endRemoveRows();
   }

   // Walk the final order once, avoiding a sorted lookup (and nickname folding)
   // for every changed peer. Adjacent rows share one notification.
   if (!changedPeers.isEmpty())
   {
      int first = -1;
      int row = 0;
      const auto flush = [&](int last) {
         if (first >= 0)
         {
            emit dataChanged(this->index(first, 0), this->index(last, this->columnCount() - 1));
            first = -1;
         }
      };
      for (Peer* peer : std::as_const(this->orderedPeers))
      {
         if (changedPeers.contains(peer))
         {
            if (first < 0)
               first = row;
         }
         else
            flush(row - 1);
         ++row;
      }
      flush(row - 1);
   }

   if (!peerIDsRemoved.isEmpty())
      emit peersRemoved(peerIDsRemoved);
}
