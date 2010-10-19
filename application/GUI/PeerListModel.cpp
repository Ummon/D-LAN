#include <PeerListModel.h>
using namespace GUI;

#include <QtAlgorithms>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

PeerListModel::PeerListModel()
{
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
      }
      else
      {
         peersToAdd << peer;
      }
   }

   if (!peersToRemove.isEmpty() || !peersToAdd.isEmpty())
      stateChanged = true;

   for (QListIterator<Peer> i(peersToRemove); i.hasNext();)
   {
      int j = this->peers.indexOf(i.next());
      if (j != -1)
      {
         this->beginRemoveRows(QModelIndex(), j, j);
         this->peers.removeAt(j);
         this->endRemoveRows();
      }
   }

   if (!peersToAdd.isEmpty())
   {
      this->beginInsertRows(QModelIndex(), this->peers.size(), this->peers.size() + peersToAdd.size() - 1);
      this->peers.append(peersToAdd);
      this->endInsertRows();
   }

   if (stateChanged)
      this->sort();
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
   if (role != Qt::DisplayRole || index.row() >= this->peers.size())
      return QVariant();

   switch (index.column())
   {
   case 0: return this->peers[index.row()].nick;
   case 1: return Common::Global::formatByteSize(this->peers[index.row()].sharingAmount);
   default: return QVariant();
   }
}

void PeerListModel::sort()
{
   emit layoutAboutToBeChanged();
   qSort(this->peers.begin(), this->peers.end(), qGreater<Peer>());
   emit layoutChanged();
}
