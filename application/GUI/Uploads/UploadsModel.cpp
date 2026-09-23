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

#include <Uploads/UploadsModel.h>
using namespace GUI;

#include <algorithm>

#include <QPixmap>
#include <QVector>

#include <Common/ProtoHelper.h>
#include <Common/Global.h>

UploadsModel::UploadsModel(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel) :
   coreConnection(coreConnection), peerListModel(peerListModel)
{
   connect(this->coreConnection.data(), &RCC::ICoreConnection::newState, this, &UploadsModel::newState);
}

int UploadsModel::rowCount(const QModelIndex& parent) const
{
   return this->uploads.size();
}

int UploadsModel::columnCount(const QModelIndex& parent) const
{
   return 3;
}

QVariant UploadsModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->uploads.size())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      {
         const Protos::GUI::State_Upload& currentUpload = this->uploads[index.row()];
         switch (index.column())
         {
         case FILENAME: return Common::ProtoHelper::getPath(currentUpload.file()).toString();
         case PROGRESS: return currentUpload.progress();
         case PEER: return this->peerListModel.getNick(currentUpload.peer_id().hash(), tr("<unknown>"));
         default: return QVariant();
         }
      }

   case Qt::TextAlignmentRole:
      return static_cast<int>(Qt::AlignLeft) | Qt::AlignVCenter;

   default: return QVariant();
   }
}

void UploadsModel::newState(const Protos::GUI::State& state)
{
   // Sort by path and then by progress (most advanced first).
   struct SortedUpload
   {
      QString path;
      const Protos::GUI::State_Upload* upload;
   };
   QVector<SortedUpload> sortedUploads;
   sortedUploads.reserve(state.uploads_size());
   for (const Protos::GUI::State_Upload& upload : state.uploads())
      sortedUploads << SortedUpload { Common::ProtoHelper::getPath(upload.file()).toString(), &upload };

   std::stable_sort(sortedUploads.begin(), sortedUploads.end(),
      [](const SortedUpload& u1, const SortedUpload& u2)
      {
         const int pathComparison = u1.path.compare(u2.path, Qt::CaseInsensitive);
         if (pathComparison != 0)
            return pathComparison < 0;
         return u1.upload->progress() > u2.upload->progress();
      }
   );

   int i = 0;
   for (; i < sortedUploads.size() && i < this->uploads.size(); i++)
   {
      if (*sortedUploads[i].upload != this->uploads[i])
      {
         this->uploads[i].CopyFrom(*sortedUploads[i].upload);
         emit dataChanged(this->createIndex(i, 0), this->createIndex(i, 2));
      }
   }

   // Insert new elements.
   if (i < sortedUploads.size())
   {
      this->beginInsertRows(QModelIndex(), i, sortedUploads.size() - 1);
      while (i < sortedUploads.size())
         this->uploads << *sortedUploads[i++].upload;
      this->endInsertRows();
   }

   // Delete some elements.
   if (i < this->uploads.size())
   {
      this->beginRemoveRows(QModelIndex(), i, this->uploads.size() - 1);
      const int nbUploads = this->uploads.size();
      while (i++ < nbUploads)
         this->uploads.removeLast();
      this->endRemoveRows();
   }
}

bool GUI::operator==(const Protos::GUI::State_Upload& u1, const Protos::GUI::State_Upload& u2)
{
   return
      u1.id() == u2.id() &&
      u1.file() == u2.file() &&
      u1.progress() == u2.progress() &&
      u1.peer_id().hash() == u2.peer_id().hash();
}

bool GUI::operator!=(const Protos::GUI::State_Upload& u1, const Protos::GUI::State_Upload& u2)
{
   return !(u1 == u2);
}
