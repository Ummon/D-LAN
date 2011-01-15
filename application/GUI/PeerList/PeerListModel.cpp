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
  
#include <PeerList/PeerListModel.h>
using namespace GUI;

#include <QtAlgorithms>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

PeerListModel::PeerListModel(QSharedPointer<RCC::ICoreConnection> coreConnection)
   : coreConnection(coreConnection)
{
   connect(this->coreConnection.data(), SIGNAL(newState(Protos::GUI::State)), this, SLOT(newState(Protos::GUI::State)));
}

/**
  * Return "<unknown>" string if not found.
  */
QString PeerListModel::getNick(const Common::Hash& peerID)
{
   for (QListIterator<Peer> i(this->peers); i.hasNext();)
   {
      Peer peer = i.next();
      if (peer.peerID == peerID)
         return peer.nick;
   }

   return "<unknown>";
}

bool PeerListModel::isOurself(int rowNum) const
{
   if (rowNum >= this->peers.size())
      return false;
   return this->peers[rowNum].peerID == this->coreConnection->getOurID();
}

Common::Hash PeerListModel::getPeerID(int rowNum) const
{
   if (rowNum >= this->peers.size())
      return Common::Hash();
   return this->peers[rowNum].peerID;
}

void PeerListModel::clear()
{
   google::protobuf::RepeatedPtrField<Protos::GUI::State_Peer> peers;
   this->setPeers(peers);
}

int PeerListModel::rowCount(const QModelIndex& parent) const
{
   return this->peers.size();
}

int PeerListModel::columnCount(const QModelIndex& parent) const
{
   return 2;
}

QVariant PeerListModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->peers.size())
      return QVariant();

   switch(role)
   {
   case Qt::DisplayRole:
      switch (index.column())
      {
      case 0: return this->peers[index.row()].nick;
      case 1: return Common::Global::formatByteSize(this->peers[index.row()].sharingAmount);
      default: return QVariant();
      }

   case Qt::TextAlignmentRole:
      return index.column() == 1 ? Qt::AlignRight : Qt::AlignLeft;

   default: return QVariant();
   }
}

void PeerListModel::newState(const Protos::GUI::State& state)
{
   // TODO : not very efficient!?
   google::protobuf::RepeatedPtrField<Protos::GUI::State_Peer> peers;
   peers.MergeFrom(state.peer());
   peers.Add()->CopyFrom(state.myself());
   this->setPeers(peers);
}

void PeerListModel::setPeers(const google::protobuf::RepeatedPtrField<Protos::GUI::State_Peer>& peers)
{
   bool stateChanged = false;

   QList<Peer> peersToRemove = this->peers;
   QList<Peer> peersToAdd;

   for (int i = 0; i < peers.size(); i++)
   {
      Peer peer =
         Peer(
            peers.Get(i).peer_id().hash().data(),
            ProtoHelper::getStr(peers.Get(i), &Protos::GUI::State_Peer::nick),
            peers.Get(i).sharing_amount()
         );

      int j = this->peers.indexOf(peer);
      if (j != -1)
      {
         peersToRemove.removeOne(peer);
         if (this->peers[j].sharingAmount != peer.sharingAmount)
         {
            this->peers[j].sharingAmount = peer.sharingAmount;
            stateChanged = true;
         }
         if (this->peers[j].nick != peer.nick)
         {
            this->peers[j].nick = peer.nick;
            stateChanged = true;
         }
      }
      else
      {
         peersToAdd << peer;
      }
   }

   if (!peersToRemove.isEmpty() || !peersToAdd.isEmpty())
      stateChanged = true;

   QList<Common::Hash> peerIDsRemoved;
   for (QListIterator<Peer> i(peersToRemove); i.hasNext();)
   {
      Peer peer = i.next();
      peerIDsRemoved << peer.peerID;
      int j = this->peers.indexOf(peer);
      if (j != -1)
      {
         this->beginRemoveRows(QModelIndex(), j, j);
         this->peers.removeAt(j);
         this->endRemoveRows();
      }
   }

   if (!peerIDsRemoved.isEmpty())
      emit peersRemoved(peerIDsRemoved);

   if (!peersToAdd.isEmpty())
   {
      this->beginInsertRows(QModelIndex(), this->peers.size(), this->peers.size() + peersToAdd.size() - 1);
      this->peers.append(peersToAdd);
      this->endInsertRows();
   }


   if (stateChanged)
      this->sort();
}

void PeerListModel::sort()
{
   emit layoutAboutToBeChanged();
   qSort(this->peers.begin(), this->peers.end(), qGreater<Peer>());
   emit layoutChanged();
}

