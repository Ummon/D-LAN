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
  
#include <Downloads/DownloadsFlatModel.h>
using namespace GUI;

#include <limits>

#include <QPixmap>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

#include <Log.h>
#include <Settings/DirListModel.h>

DownloadsFlatModel::DownloadsFlatModel(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const IFilter<DownloadFilterStatus>& filter) :
   DownloadsModel(coreConnection, peerListModel, sharedDirsModel, filter),
   totalBytesInQueue(0),
   totalBytesDownloadedInQueue(0),
   eta(0)
{
}

quint64 DownloadsFlatModel::getTotalBytesInQueue() const
{
   return this->totalBytesInQueue;
}

quint64 DownloadsFlatModel::getTotalBytesDownloadedInQueue() const
{
   return this->totalBytesDownloadedInQueue;
}

quint64 DownloadsFlatModel::getEta() const
{
   return this->eta;
}

QList<quint64> DownloadsFlatModel::getDownloadIDs(const QModelIndex& index) const
{
   if (index.row() >= this->downloads.size())
      return QList<quint64>();
   return QList<quint64>() << this->downloads[index.row()].id();
}

bool DownloadsFlatModel::isDownloadPaused(const QModelIndex& index) const
{
   if (index.row() >= this->downloads.size())
      return false;
   return this->downloads[index.row()].status() == Protos::GUI::State::Download::PAUSED;
}

bool DownloadsFlatModel::isFileLocationKnown(const QModelIndex& index) const
{
   if (index.row() >= this->downloads.size())
      return false;

   // If we know the base path then we know the location of the file.
   return this->downloads[index.row()].local_entry().exists();
}

bool DownloadsFlatModel::isFileComplete(const QModelIndex& index) const
{
   if (index.row() >= this->downloads.size())
      return false;

   return this->downloads[index.row()].status() == Protos::GUI::State_Download_Status_COMPLETE;
}

bool DownloadsFlatModel::isSourceAlive(const QModelIndex& index) const
{
   if (index.row() >= this->downloads.size())
      return false;

   return this->downloads[index.row()].peer_id_size() > 0 && !this->peerListModel.getNick(this->downloads[index.row()].peer_id(0).hash()).isNull();
}

Protos::Common::Entry::Type DownloadsFlatModel::getType(const QModelIndex& index) const
{
   if (index.row() >= this->downloads.size())
      return Protos::Common::Entry::FILE;

   return this->downloads[index.row()].local_entry().type();
}

QString DownloadsFlatModel::getPath(const QModelIndex& index, bool appendFilename) const
{
   if (index.row() >= this->downloads.size())
      return QString();

   const Common::SharedDir sharedDir = this->sharedDirsModel.getDir(this->downloads[index.row()].local_entry().shared_dir().id().hash());
   if (sharedDir.isNull())
      return QString();

   QString path = sharedDir.path.left(sharedDir.path.count() - 1);
   return path.append(Common::ProtoHelper::getRelativePath(this->downloads[index.row()].local_entry(), appendFilename));
}

int DownloadsFlatModel::rowCount(const QModelIndex& parent) const
{
   if (parent.isValid())
      return 0;

   return this->downloads.size();
}

QVariant DownloadsFlatModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->downloads.size())
      return QVariant();

   return DownloadsModel::getData(this->downloads[index.row()], index, role);
}

Qt::DropActions DownloadsFlatModel::supportedDropActions() const
{
   return Qt::MoveAction;
}

Qt::ItemFlags DownloadsFlatModel::flags(const QModelIndex& index) const
{
   Qt::ItemFlags defaultFlags = QAbstractItemModel::flags(index);

   if (index.isValid())
       return Qt::ItemIsDragEnabled | defaultFlags;
   else
       return Qt::ItemIsDropEnabled | defaultFlags;
}

bool DownloadsFlatModel::dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int /*column*/, const QModelIndex& /*parent*/)
{
   if (row == -1 || !data || action != Qt::MoveAction ||  this->downloads.isEmpty())
       return false;

   QList<int> rows = this->getDraggedRows(data);
   if (rows.isEmpty())
      return false;


   // Defines the reference ID.
   Protos::GUI::MoveDownloads::Position position = Protos::GUI::MoveDownloads::BEFORE;
   quint64 placeToMove = 0;
   if (row >= this->downloads.size())
   {
      position = Protos::GUI::MoveDownloads::AFTER;
      placeToMove = this->downloads.last().id();
   }
   else
      placeToMove = this->downloads[row].id();

   // Defines the download IDs to move.
   QList<quint64> downloadIDs;
   for (QListIterator<int> i(rows); i.hasNext();)
   {
      int currentRow = i.next();
      if (currentRow < this->downloads.size())
         downloadIDs << this->downloads[currentRow].id();
   }

   // We remove the moved download from the list (not necessery but nicer for the user experience).
   if (!rows.isEmpty())
   {
      qSort(rows.begin(), rows.end());

      int rowBegin = rows.size() - 1;
      int rowEnd = rowBegin;
      for (int i = rowEnd - 1; i >= -1 ; i--)
      {
         if (i >= 0 && rows[i] == rows[rowBegin] - 1)
            rowBegin--;
         else
         {
            this->beginRemoveRows(QModelIndex(), rows[rowBegin], rows[rowEnd]);
            for (int j = rows[rowEnd]; j >= rows[rowBegin]; j--)
               this->downloads.removeAt(j);
            this->endRemoveRows();

            rowBegin = rowEnd = i;
         }
      }
   }

   this->coreConnection->moveDownloads(placeToMove, downloadIDs, position);
   return true;
}

void DownloadsFlatModel::onNewState(const Protos::GUI::State& state)
{
   const quint64 oldTotalBytesInQueue = this->totalBytesInQueue;
   const quint64 oldTotalBytesDownloadedInQueue = this->totalBytesDownloadedInQueue;

   this->totalBytesInQueue = 0;
   this->totalBytesDownloadedInQueue = 0;

   for (int i = 0; i < state.download_size(); i++)
   {
      this->totalBytesInQueue += state.download(i).local_entry().size();
      this->totalBytesDownloadedInQueue += state.download(i).downloaded_bytes();
   }

   QList<int> activeDownloadIndices = this->getNonFilteredDownloadIndices(state);

   int i = 0;
   for (; i < activeDownloadIndices.size() && i < this->downloads.size(); i++)
   {
      if (state.download(activeDownloadIndices[i]) != this->downloads[i])
      {
         this->downloads[i].CopyFrom(state.download(activeDownloadIndices[i]));
         emit dataChanged(this->createIndex(i, 0), this->createIndex(i, 3));
      }
   }

   // Insert new elements.
   if (i < activeDownloadIndices.size())
   {
      this->beginInsertRows(QModelIndex(), i, activeDownloadIndices.size() - 1);
      while (i < activeDownloadIndices.size())
      {
         const Protos::GUI::State_Download& download = state.download(activeDownloadIndices[i++]);
         this->downloads << download;
      }
      this->endInsertRows();
   }

   // Delete some elements.
   if (i < this->downloads.size())
   {
      this->beginRemoveRows(QModelIndex(), i, this->downloads.size() - 1);
      const int nbDownloads = this->downloads.size();
      while (i++ < nbDownloads)
         this->downloads.removeLast();
      this->endRemoveRows();
   }

   quint64 oldEta = this->eta;
   if (state.stats().download_rate() == 0)
      this->eta = std::numeric_limits<quint64>::max();
   else
   {
      const int weightLastEta = this->eta == 0 ? 1 : WEIGHT_LAST_ETA;
      this->eta = (weightLastEta * this->eta + (this->totalBytesInQueue - this->totalBytesDownloadedInQueue) / state.stats().download_rate()) / (weightLastEta + 1);
   }

   if (this->totalBytesInQueue != oldTotalBytesInQueue || this->totalBytesDownloadedInQueue != oldTotalBytesDownloadedInQueue || this->eta != oldEta)
      emit globalProgressChanged();
}
