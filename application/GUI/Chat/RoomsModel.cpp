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
   connect(coreConnection.data(), SIGNAL(newState(const Protos::GUI::State&)), this, SLOT(newState(const Protos::GUI::State&)));
   connect(coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));
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

int RoomsModel::rowCount(const QModelIndex& /*parent*/) const
{
   return this->orderedRooms.size();
}

int RoomsModel::columnCount(const QModelIndex& /*parent*/) const
{
   return 2;
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
      case 1: return room->peerIDs.size() + (room->joined ? 1 : 0);
      }
      break;
   }
   case Qt::TextAlignmentRole:
      return (index.column() == 1 ? Qt::AlignRight : Qt::AlignLeft) + Qt::AlignVCenter;
   }

   return QVariant();
}

QString RoomsModel::getRoomName(const QModelIndex& index)
{
   if (index.row() < this->orderedRooms.size())
      return this->orderedRooms.getFromIndex(index.row())->name;

   return QString();
}

void RoomsModel::setSortType(Protos::GUI::Settings::RoomSortType sortType)
{
   this->currentSortType = sortType;

   emit layoutAboutToBeChanged();
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
         if (r1->peerIDs.count() == r2->peerIDs.count())
            return r1->name < r2->name;
         return r1->peerIDs.count() > r2->peerIDs.count();
      });
      break;
   }
   emit layoutChanged();
}

Protos::GUI::Settings::RoomSortType RoomsModel::getSortType() const
{
   return this->currentSortType;
}

void RoomsModel::newState(const Protos::GUI::State& state)
{
   this->updateRooms(state.room());
}

void RoomsModel::coreDisconnected(bool force)
{
   google::protobuf::RepeatedPtrField<Protos::GUI::State_Room> rooms;
   this->updateRooms(rooms);
}

void RoomsModel::updateRooms(const google::protobuf::RepeatedPtrField<Protos::GUI::State::Room>& rooms)
{
   Common::SortedArray<Room*> roomsToRemove = this->orderedRooms;
   QList<Room*> roomsToAdd;

   for (int i = 0; i < rooms.size(); i++)
   {
      QSet<Common::Hash> peerIDs;
      for (int j = 0; j < rooms.Get(i).peer_id_size(); j++)
         peerIDs << Common::Hash(rooms.Get(i).peer_id(j).hash());
      const QString& name = Common::ProtoHelper::getStr(rooms.Get(i), &Protos::GUI::State::Room::name);
      const bool joined = rooms.Get(i).joined();

      auto roomIterator = this->indexedRooms.find(name);
      Room* room = roomIterator == this->indexedRooms.end() ? nullptr : *roomIterator;
      int j;
      if (room && (j = this->orderedRooms.indexOf(room)) != -1)
      {
         roomsToRemove.remove(room);
         if (room->peerIDs != peerIDs)
         {
            if (this->currentSortType == Protos::GUI::Settings::BY_NB_PEERS)
            {
               this->beginRemoveRows(QModelIndex(), j, j);
               this->orderedRooms.remove(room);
               this->endRemoveRows();
               room->peerIDs = peerIDs;
               roomsToAdd << room;
            }
            else
            {
               room->peerIDs = peerIDs;
               emit dataChanged(this->createIndex(j, 1), this->createIndex(j, 1));
            }
         }

         if (room->joined != joined)
         {
            room->joined = joined;
            emit dataChanged(this->createIndex(j, 0), this->createIndex(j, 0));
         }
      }
      else
      {
         roomsToAdd << new Room { name, peerIDs, joined };
      }
   }

   QList<QString> roomNamesRemoved;
   auto end = roomsToRemove.end();
   for (auto i = roomsToRemove.begin(); i != end; ++i)
   {
      Room* const room = *i;
      roomNamesRemoved << room->name;
      int j = this->orderedRooms.indexOf(room);
      if (j != -1)
      {
         this->beginRemoveRows(QModelIndex(), j, j);
         this->indexedRooms.remove(room->name);
         this->orderedRooms.remove(room);
         delete room;
         this->endRemoveRows();
      }
   }

   for (QListIterator<Room*> i(roomsToAdd); i.hasNext();)
   {
      Room* const room = i.next();
      int pos = this->orderedRooms.insert(room);
      this->beginInsertRows(QModelIndex(), pos, pos);
      this->indexedRooms.insert(room->name, room);
      this->endInsertRows();
   }
}
