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
  
#include <Downloads/DownloadsModel.h>
using namespace GUI;

#include <QPixmap>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

#include <Log.h>

DownloadsModel::DownloadsModel(CoreConnection& coreConnection, PeerListModel& peerListModel, const IFilter<DownloadFilterStatus>& filter)
   : coreConnection(coreConnection), peerListModel(peerListModel), filter(filter)
{
   qRegisterMetaTypeStreamOperators<Progress>("Progress"); // Don't know where to put this call..
   connect(&this->coreConnection, SIGNAL(newState(Protos::GUI::State)), this, SLOT(newState(Protos::GUI::State)));
}

quint64 DownloadsModel::getDownloadID(int row) const
{
   if (row >= this->downloads.size())
      return 0;
   return this->downloads[row].id();
}

QList<quint64> DownloadsModel::getCompletedDownloadIDs() const
{
   QList<quint64> IDs;
   for (int i = 0; i < this->downloads.size(); i++)
      if (this->downloads[i].status() == Protos::GUI::State_Download_Status_COMPLETE)
         IDs << this->downloads[i].id();
   return IDs;
}

bool DownloadsModel::fileLocationIsKnown(int row) const
{
   if (row >= this->downloads.size())
      return false;

   // If we know the base path then we know the location of the file.
   return !Common::ProtoHelper::getStr(this->downloads[row], &Protos::GUI::State_Download::base_path).isEmpty();
}

QString DownloadsModel::getLocationPath(int row) const
{
   if (row >= this->downloads.size())
      return QString();

   QString fullPath;
   fullPath
      .append(Common::ProtoHelper::getStr(this->downloads[row], &Protos::GUI::State_Download::base_path)) // Base path
      .append(Common::ProtoHelper::getStr(this->downloads[row].entry(), &Protos::Common::Entry::path)); // Relative path from base path
   return fullPath;
}

int DownloadsModel::rowCount(const QModelIndex& parent) const
{
   return this->downloads.size();
}

int DownloadsModel::columnCount(const QModelIndex& parent) const
{
   return 4;
}

QVariant DownloadsModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->downloads.size())
      return QVariant();

   switch(role)
   {
   case Qt::DisplayRole:
      {
         const Protos::GUI::State_Download& currentDownload = this->downloads[index.row()];
         switch (index.column())
         {
         case 0: return Common::ProtoHelper::getStr(currentDownload.entry(), &Protos::Common::Entry::name);
         case 1: return Common::Global::formatByteSize(currentDownload.entry().size());
         case 2: return QVariant::fromValue(Progress(currentDownload.progress(), currentDownload.status()));
         case 3:
            {
               QString peersStr;
               // O(n^2).
               for (int i = 0; i < currentDownload.peer_id_size(); i++)
               {
                  if (i != 0)
                     peersStr.append(" ");
                  peersStr.append(this->peerListModel.getNick(currentDownload.peer_id(i).hash().data()));
               }
               return peersStr;
            }
         default: return QVariant();
         }
      }

   case Qt::DecorationRole:
      {
         if (index.column() == 0)
         {
            if (this->downloads[index.row()].status() >= Protos::GUI::State_Download_Status_UNKNOWN_PEER)
               return QPixmap(":/icons/ressources/error.png");
            else if (this->downloads[index.row()].entry().type() == Protos::Common::Entry_Type_DIR)
               return QPixmap(":/icons/ressources/folder.png");
            else
               return QPixmap(":/icons/ressources/file.png");
         }
         return QVariant();
      }

   case Qt::ToolTipRole:
      switch(this->downloads[index.row()].status())
      {
      case Protos::GUI::State_Download_Status_UNKNOWN_PEER:
         return "Unknown source peer";
      case Protos::GUI::State_Download_Status_ENTRY_NOT_FOUND:
         return "The source peer doesn't have the entry";
      case Protos::GUI::State_Download_Status_NO_SOURCE:
         return "There is no source to download from";
      case Protos::GUI::State_Download_Status_NO_SHARED_DIRECTORY_TO_WRITE:
         return "No incoming folder";
      case Protos::GUI::State_Download_Status_NO_ENOUGH_FREE_SPACE:
         return "Not enough free space left";
      case Protos::GUI::State_Download_Status_UNABLE_TO_CREATE_THE_FILE:
         return "Unable to create the file";
      case Protos::GUI::State_Download_Status_UNABLE_TO_RETRIEVE_THE_HASHES:
         return "Unable to retrieve the hashes";
      default:
         return QVariant();
      }

   case Qt::TextAlignmentRole:
      return index.column() == 1 ? Qt::AlignRight : Qt::AlignLeft;

   default: return QVariant();
   }
}

Qt::DropActions DownloadsModel::supportedDropActions() const
{
   return Qt::MoveAction;
}

Qt::ItemFlags DownloadsModel::flags(const QModelIndex& index) const
{
   Qt::ItemFlags defaultFlags = QAbstractItemModel::flags(index);

   if (index.isValid())
       return Qt::ItemIsDragEnabled | defaultFlags;
   else
       return Qt::ItemIsDropEnabled | defaultFlags;

   return defaultFlags;
}

bool DownloadsModel::dropMimeData(const QMimeData* data, Qt::DropAction action, int row, int column, const QModelIndex & parent)
{
   if (row == -1 || !data || action != Qt::MoveAction)
       return false;

   QStringList types = this->mimeTypes();
   if (types.isEmpty())
       return false;

   QString format = types.at(0);
   if (!data->hasFormat(format))
       return false;

   if (this->downloads.isEmpty())
      return false;

   QByteArray encoded = data->data(format);
   QDataStream stream(&encoded, QIODevice::ReadOnly);

   QList<quint64> downloadIDs;
   QList<int> rowsToRemove;

   bool moveBefore = true;
   quint64 placeToMove = 0;
   if (row >= this->downloads.size())
   {
      moveBefore = false;
      placeToMove = this->downloads.last().id();
   }
   else
      placeToMove = this->downloads[row].id();


   int previousRow = -1;
   while (!stream.atEnd())
   {
       int currentRow;
       int currentCol;
       QMap<int, QVariant> value;
       stream >> currentRow >> currentCol >> value;

       if (currentRow != previousRow)
       {
          previousRow = currentRow;
          if (currentRow >= 0 && currentRow < this->downloads.size())
          {
             downloadIDs << this->downloads[currentRow].id();
             rowsToRemove << currentRow;
          }
       }
   }

   if (!rowsToRemove.isEmpty())
   {
      qSort(rowsToRemove.begin(), rowsToRemove.end());

      int rowBegin = rowsToRemove.size() - 1;
      int rowEnd = rowBegin;
      for (int i = rowEnd - 1; i >= -1 ; i--)
      {
         if (i >= 0 && rowsToRemove[i] == rowsToRemove[rowBegin] - 1)
            rowBegin--;
         else
         {
            this->beginRemoveRows(QModelIndex(), rowsToRemove[rowBegin], rowsToRemove[rowEnd]);
            for (int j = rowsToRemove[rowEnd]; j >= rowsToRemove[rowBegin]; j--)
               this->downloads.removeAt(j);
            this->endRemoveRows();

            rowBegin = rowEnd = i;
         }
      }
   }

   this->coreConnection.moveDownloads(placeToMove, downloadIDs, moveBefore);
   return true;
}

void DownloadsModel::newState(const Protos::GUI::State& state)
{
   int statusToFilter = 0;
   const QList<DownloadFilterStatus>& filterStatus = this->filter.getFilteredValues();
   for (int i = 0; i < filterStatus.size(); i++)
      statusToFilter |= filterStatus[i];

   QList<int> indexToInsert;
   for (int i = 0; i < state.download_size(); i++)
   {
      switch (state.download(i).status())
      {
      case Protos::GUI::State_Download_Status_QUEUED:
      case Protos::GUI::State_Download_Status_PAUSED:
         if (!(statusToFilter & STATUS_QUEUED))
            indexToInsert << i;
         break;

      case Protos::GUI::State_Download_Status_INITIALIZING:
      case Protos::GUI::State_Download_Status_DOWNLOADING:
         if (!(statusToFilter & STATUS_DOWNLOADING))
            indexToInsert << i;
         break;

      case Protos::GUI::State_Download_Status_COMPLETE:
         if (!(statusToFilter & STATUS_COMPLETE))
            indexToInsert << i;
         break;

      case Protos::GUI::State_Download_Status_UNKNOWN_PEER:
      case Protos::GUI::State_Download_Status_ENTRY_NOT_FOUND:
      case Protos::GUI::State_Download_Status_NO_SOURCE:
      case Protos::GUI::State_Download_Status_NO_SHARED_DIRECTORY_TO_WRITE:
      case Protos::GUI::State_Download_Status_NO_ENOUGH_FREE_SPACE:
      case Protos::GUI::State_Download_Status_UNABLE_TO_CREATE_THE_FILE:
      case Protos::GUI::State_Download_Status_UNABLE_TO_RETRIEVE_THE_HASHES:
         if (!(statusToFilter & STATUS_ERROR))
            indexToInsert << i;
         break;
      }
   }

   int i = 0;
   for (; i < indexToInsert.size() && i < this->downloads.size(); i++)
   {
      if (state.download(indexToInsert[i]) != this->downloads[i])
      {
         this->downloads[i].CopyFrom(state.download(indexToInsert[i]));
         emit dataChanged(this->createIndex(i, 0), this->createIndex(i, 3));
      }
   }

   // Insert new elements.
   if (i < indexToInsert.size())
   {
      this->beginInsertRows(QModelIndex(), i, indexToInsert.size() - 1);
      while (i < indexToInsert.size())
      {
         const Protos::GUI::State_Download& download = state.download(indexToInsert[i++]);
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
}

/**
  * Used when drag'n dropping some downloads.
  */
QDataStream& GUI::operator<<(QDataStream& out, const Progress& progress)
{
   out << progress.progress;
   out << static_cast<int>(progress.status);
   return out;
}

/**
  * Used when drag'n dropping some downloads.
  */
QDataStream& GUI::operator>>(QDataStream& in, Progress& progress)
{
   in >> progress.progress;
   int status;
   in >> status;
   progress.status = static_cast<Protos::GUI::State_Download_Status>(status);
   return in;
}

bool GUI::operator==(const Protos::GUI::State_Download& d1, const Protos::GUI::State_Download& d2)
{
   if (
      d1.id() != d2.id() ||
      d1.entry().type() != d2.entry().type() ||
      d1.entry().path() != d2.entry().path() ||
      d1.entry().name() != d2.entry().name() ||
      d1.entry().size() != d2.entry().size() ||
      d1.status() != d2.status() ||
      d1.progress() != d2.progress() ||
      d1.peer_id_size() != d2.peer_id_size()
   )
      return false;

   for (int i = 0; i < d1.peer_id_size(); i++)
      if (d1.peer_id(i).hash() != d2.peer_id(i).hash())
         return false;

   return true;
}

bool GUI::operator!=(const Protos::GUI::State_Download& d1, const Protos::GUI::State_Download& d2)
{
   return !(d1 == d2);
}
