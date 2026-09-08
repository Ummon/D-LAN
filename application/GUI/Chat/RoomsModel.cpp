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

#include <Chat/RoomsModel.h>
using namespace GUI;

#include <Common/ProtoHelper.h>

#include <Log.h>

struct RoomsModel::Room
{
   bool operator==(const Room& r) const { return this->name == r.name; }
   bool operator!=(const Room& r) const { return this->name != r.name; }
   qsizetype peerCount() const { return this->peerIDs.size() + (this->joined ? 1 : 0); }

   QString name; // Room identifier.
   QSet<Common::Hash> peerIDs;
   bool joined;
};

/**
  * @class RoomsModel
  *
  * The list of all rooms.
  */

RoomsModel::RoomsModel(QSharedPointer<RCC::ICoreConnection> coreConnection) :
   coreConnection(coreConnection),
   currentSortType(Protos::GUI::Settings::BY_NAME)
{
   this->setSortType(this->currentSortType);
   connect(coreConnection.data(), &RCC::ICoreConnection::newState, this, &RoomsModel::newState);
   connect(coreConnection.data(), &RCC::ICoreConnection::disconnected, this, &RoomsModel::coreDisconnected);
}

RoomsModel::~RoomsModel()
{
   auto end = this->orderedRooms.end();
   for (auto i = this->orderedRooms.begin(); i != end; ++i)
      delete *i;
}

/*QModelIndex	RoomsModel::index(int row, int column, const QModelIndex& parent) const
{
   return this->createIndex(row, column)
}*/

int RoomsModel::rowCount(const QModelIndex& parent) const
{
   return parent.isValid() ? 0 : this->orderedRooms.size();
}

int RoomsModel::columnCount(const QModelIndex& parent) const
{
   return parent.isValid() ? 0 : 2;
}

QVariant RoomsModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->orderedRooms.size())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
   {
      const Room* room = this->orderedRooms.getFromIndex(index.row());
      switch (index.column())
      {
      case 0: return room->name;
      case 1: return room->peerCount();
      }
      break;
   }
   case Qt::TextAlignmentRole:
      return QVariant((index.column() == 1 ? Qt::AlignRight : Qt::AlignLeft) | Qt::AlignVCenter);
   }

   return QVariant();
}

/**
  * Returns an empty name if the index doesn't correspond to a room, an invalid index for example: the row of
  * such an index is -1, which passes the upper bound check and makes 'getFromIndex(..)' throw.
  */
QString RoomsModel::getRoomName(const QModelIndex& index)
{
   if (index.row() >= 0 && index.row() < this->orderedRooms.size())
      return this->orderedRooms.getFromIndex(index.row())->name;

   return QString();
}

void RoomsModel::setSortType(Protos::GUI::Settings::RoomSortType sortType)
{
   if (sortType != Protos::GUI::Settings::BY_NAME && sortType != Protos::GUI::Settings::BY_NB_PEERS)
      return;

   emit layoutAboutToBeChanged();
   // Views may create persistent indexes in response to layoutAboutToBeChanged.
   const QModelIndexList oldIndexes = this->persistentIndexList();
   QList<Room*> indexedRooms;
   for (const QModelIndex& index : oldIndexes)
      indexedRooms.append(this->orderedRooms.getFromIndex(index.row()));

   this->currentSortType = sortType;
   switch (this->currentSortType)
   {
   case Protos::GUI::Settings::BY_NAME:
      this->orderedRooms.setSortedFunction([](const Room* r1, const Room* r2) {
         if (!r1 || !r2)
            return false;
         return r1->name < r2->name;
      });
      break;

   case Protos::GUI::Settings::BY_NB_PEERS:
      this->orderedRooms.setSortedFunction([](const Room* r1, const Room* r2) {
         if (!r1 || !r2)
            return false;
         if (r1->peerCount() == r2->peerCount())
            return r1->name < r2->name;
         return r1->peerCount() > r2->peerCount();
      });
      break;

   default:;
   }

   QModelIndexList newIndexes;
   for (int i = 0; i < oldIndexes.size(); ++i)
      newIndexes.append(this->index(this->orderedRooms.indexOf(indexedRooms[i]), oldIndexes[i].column()));
   this->changePersistentIndexList(oldIndexes, newIndexes);
   emit layoutChanged();
}

Protos::GUI::Settings::RoomSortType RoomsModel::getSortType() const
{
   return this->currentSortType;
}

void RoomsModel::newState(const Protos::GUI::State& state)
{
   this->updateRooms(state.rooms());
}

void RoomsModel::coreDisconnected(bool force)
{
   google::protobuf::RepeatedPtrField<Protos::GUI::State_Room> rooms;
   this->updateRooms(rooms);
}

void RoomsModel::updateRooms(const google::protobuf::RepeatedPtrField<Protos::GUI::State::Room>& rooms)
{
   QList<QString> roomsToRemoveList = this->indexedRooms.keys();
   QSet<QString> roomsToRemove(roomsToRemoveList.begin(), roomsToRemoveList.end());

   for (int i = 0; i < rooms.size(); i++)
   {
      QSet<Common::Hash> peerIDs;
      for (int j = 0; j < rooms.Get(i).peer_ids_size(); j++)
         peerIDs << Common::Hash(rooms.Get(i).peer_ids(j).hash());
      const QString name = QString::fromStdString(rooms.Get(i).name());
      const bool joined = rooms.Get(i).joined();

      auto roomIterator = this->indexedRooms.find(name);
      Room* room = roomIterator == this->indexedRooms.end() ? nullptr : *roomIterator;
      if (room)
      {
         roomsToRemove.remove(name);

         if (room->peerIDs != peerIDs || room->joined != joined)
         {
            const int oldRow = this->orderedRooms.indexOf(room);
            Room updatedRoom { name, peerIDs, joined };
            // Calculate the destination without changing the live model before beginMoveRows.
            auto updatedOrder = this->orderedRooms;
            updatedOrder.remove(room);
            const int newRow = updatedOrder.insert(&updatedRoom);
            if (oldRow != newRow)
               this->beginMoveRows(QModelIndex(), oldRow, oldRow, QModelIndex(), newRow > oldRow ? newRow + 1 : newRow);

            this->orderedRooms.remove(room);
            room->peerIDs = peerIDs;
            room->joined = joined;
            this->orderedRooms.insert(room);

            if (oldRow != newRow)
               this->endMoveRows();
            emit dataChanged(this->index(newRow, 0), this->index(newRow, 1));
         }
      }
      else
      {
         Room* r = new Room { name, peerIDs, joined };
         auto updatedOrder = this->orderedRooms;
         const int row = updatedOrder.insert(r);
         this->beginInsertRows(QModelIndex(), row, row);
         this->indexedRooms.insert(name, r);
         this->orderedRooms = updatedOrder;
         this->endInsertRows();
      }
   }

   for (auto i = roomsToRemove.begin(); i != roomsToRemove.end(); ++i)
   {
      Room* room = this->indexedRooms[*i];
      const int row = this->orderedRooms.indexOf(room);
      this->beginRemoveRows(QModelIndex(), row, row);
      this->indexedRooms.remove(room->name);
      this->orderedRooms.remove(room);
      delete room;
      this->endRemoveRows();
   }
}
