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
#include <algorithm>

#include <QPixmap>
#include <QSet>

#include <Protos/common.pb.h>
#include <Common/ProtoHelper.h>
#include <Common/Global.h>

#include <Log.h>
#include <Settings/SharedEntryListModel.h>

DownloadsFlatModel::DownloadsFlatModel(
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   const PeerListModel& peerListModel,
   const SharedEntryListModel& sharedEntryListModel,
   const IFilter<DownloadFilterStatus>& filter
) :
   DownloadsModel(coreConnection, peerListModel, sharedEntryListModel, filter),
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
   return this->downloads[index.row()].status() == Protos::Common::DownloadStatus::PAUSED;
}

bool DownloadsFlatModel::isEntryLocationKnown(const QModelIndex& index) const
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

   return this->downloads[index.row()].status() == Protos::Common::DownloadStatus::COMPLETE;
}

bool DownloadsFlatModel::isSourceAlive(const QModelIndex& index) const
{
   if (index.row() >= this->downloads.size())
      return false;

   return
      this->downloads[index.row()].peer_ids_size() > 0 &&
      !this->peerListModel.getNick(this->downloads[index.row()].peer_ids(0).hash()).isNull();
}

Protos::Common::Entry::Type DownloadsFlatModel::getType(const QModelIndex& index) const
{
   if (index.row() >= this->downloads.size())
      return Protos::Common::Entry::FILE;

   return this->downloads[index.row()].local_entry().type();
}

/**
  * Returns the absolute local path.
  */
QString DownloadsFlatModel::getPath(const QModelIndex& index, bool appendFilename) const
{
   if (index.row() >= this->downloads.size())
      return QString();

   const Common::SharedEntry& sharedEntry =
      this->sharedEntryListModel.getSharedEntry(this->downloads[index.row()].local_entry().shared_entry().id().hash());

   if (sharedEntry.isNull())
      return QString();

   if (sharedEntry.path.isFile())
   {
      return sharedEntry.path.toString(appendFilename);
   }
   else
   {
      return DownloadsModel::getExistingPathOrParentDirectory(
         sharedEntry.path.append(Common::ProtoHelper::getPath(this->downloads[index.row()].local_entry())),
         appendFilename
      );
   }
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

bool DownloadsFlatModel::dropMimeData(
   const QMimeData* data,
   Qt::DropAction action,
   int row,
   int /*column*/,
   const QModelIndex& /*parent*/
)
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

   // We remove the moved download from the list (not necessary but nicer for the user experience).
   if (!rows.isEmpty())
   {
      std::sort(rows.begin(), rows.end());

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

   for (int i = 0; i < state.downloads_size(); i++)
   {
      this->totalBytesInQueue += state.downloads(i).local_entry().size();
      this->totalBytesDownloadedInQueue += state.downloads(i).downloaded_bytes();
   }

   const QList<int> activeDownloadIndices = this->getNonFilteredDownloadIndices(state);

   QSet<quint64> newIDs;
   newIDs.reserve(activeDownloadIndices.size());
   for (int i = 0; i < activeDownloadIndices.size(); i++)
      newIDs.insert(state.downloads(activeDownloadIndices[i]).id());

   // 1) Remove the rows which no longer exist, by contiguous ranges. Done from the end to keep the positions valid.
   //    For example when the completed downloads are removed by the user.
   for (int i = this->downloads.size() - 1; i >= 0; i--)
   {
      if (newIDs.contains(this->downloads[i].id()))
         continue;

      const int last = i;
      while (i > 0 && !newIDs.contains(this->downloads[i - 1].id()))
         i--;

      this->beginRemoveRows(QModelIndex(), i, last);
      this->downloads.remove(i, last - i + 1);
      this->endRemoveRows();
   }

   QSet<quint64> oldIDs;
   oldIDs.reserve(this->downloads.size());
   for (int i = 0; i < this->downloads.size(); i++)
      oldIDs.insert(this->downloads[i].id());

   // 2) Walk both lists together: the existing rows are updated and the new rows are inserted by contiguous ranges.
   //    The consecutive modified rows are notified with a single 'dataChanged(..)'.
   //    If the order of the existing rows has changed (some downloads have been moved) the rows are simply rewritten
   //    from that point.
   int firstModified = -1; // First row of the current range of modified rows, -1 if none.
   auto flushModified = [&](int lastModified)
   {
      if (firstModified != -1)
      {
         emit dataChanged(this->createIndex(firstModified, 0), this->createIndex(lastModified, this->columnCount() - 1));
         firstModified = -1;
      }
   };
   auto overwriteRow = [&](int i, const Protos::GUI::State::Download& download)
   {
      if (this->downloads[i] != download)
      {
         this->downloads[i].CopyFrom(download);
         if (firstModified == -1)
            firstModified = i;
      }
      else
         flushModified(i - 1);
   };

   bool orderChanged = false;
   int i = 0; // Current position, 'this->downloads' and 'activeDownloadIndices' are kept synchronized below 'i'.
   while (i < activeDownloadIndices.size())
   {
      const Protos::GUI::State::Download& download = state.downloads(activeDownloadIndices[i]);

      if (!orderChanged && i < this->downloads.size() && this->downloads[i].id() == download.id())
      {
         overwriteRow(i, download);
         i++;
      }
      else if (!orderChanged && !oldIDs.contains(download.id()))
      {
         flushModified(i - 1);

         int last = i;
         while (last + 1 < activeDownloadIndices.size() && !oldIDs.contains(state.downloads(activeDownloadIndices[last + 1]).id()))
            last++;

         this->beginInsertRows(QModelIndex(), i, last);
         for (int j = i; j <= last; j++)
            this->downloads.insert(j, state.downloads(activeDownloadIndices[j]));
         this->endInsertRows();

         i = last + 1;
      }
      else
      {
         orderChanged = true;

         if (i < this->downloads.size())
         {
            overwriteRow(i, download);
            i++;
         }
         else // Append all the remaining rows.
         {
            flushModified(i - 1);

            this->beginInsertRows(QModelIndex(), i, activeDownloadIndices.size() - 1);
            for (; i < activeDownloadIndices.size(); i++)
               this->downloads << state.downloads(activeDownloadIndices[i]);
            this->endInsertRows();
         }
      }
   }
   flushModified(i - 1);

   // 3) Remove the extra rows at the end, only possible if the order has changed.
   if (i < this->downloads.size())
   {
      this->beginRemoveRows(QModelIndex(), i, this->downloads.size() - 1);
      this->downloads.remove(i, this->downloads.size() - i);
      this->endRemoveRows();
   }

   quint64 oldEta = this->eta;
   if (state.stats().download_rate() == 0)
      this->eta = std::numeric_limits<quint64>::max();
   else
   {
      const int weightLastEta = this->eta == 0 ? 1 : WEIGHT_LAST_ETA;
      this->eta =
         (
            weightLastEta * this->eta +
            (this->totalBytesInQueue - this->totalBytesDownloadedInQueue) / state.stats().download_rate()
         ) / (weightLastEta + 1);
   }

   if (
      this->totalBytesInQueue != oldTotalBytesInQueue ||
      this->totalBytesDownloadedInQueue != oldTotalBytesDownloadedInQueue ||
      this->eta != oldEta
   )
      emit globalProgressChanged();
}
