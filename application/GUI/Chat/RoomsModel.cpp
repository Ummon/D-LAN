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

/**
  * @class RoomsModel
  *
  * The list of all rooms.
  */

RoomsModel::RoomsModel(QSharedPointer<RCC::ICoreConnection> coreConnection) :
   coreConnection(coreConnection)
{
   connect(coreConnection.data(), SIGNAL(newState(const Protos::GUI::State&)), this, SLOT(newState(const Protos::GUI::State&)));
   connect(coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));
}

/*QModelIndex	RoomsModel::index(int row, int column, const QModelIndex& parent) const
{
   return this->createIndex(row, column)
}*/

int RoomsModel::rowCount(const QModelIndex& /*parent*/) const
{
   return this->rooms.size();
}

int RoomsModel::columnCount(const QModelIndex& /*parent*/) const
{
   return 2;
}

QVariant RoomsModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->rooms.size())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
   {
      const Room& room = this->rooms.getFromIndex(index.row());
      switch (index.column())
      {
      case 0: return room.name;
      case 1: return room.peerIDs.size() + (room.joined ? 1 : 0);
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
   if (index.row() < this->rooms.size())
   {
      return this->rooms.getFromIndex(index.row()).name;
   }

   return QString();
}

void RoomsModel::newState(const Protos::GUI::State& state)
{
   Common::SortedArray<Room> roomsNewState;

   for (int i = 0; i < state.rooms_size(); i++)
   {
      const Protos::GUI::State::Room& room = state.rooms(i);
      QSet<Common::Hash> peers;
      for (int j = 0; j < room.peer_id_size(); j++)
         peers << Common::Hash(room.peer_id(j).hash());
      roomsNewState.insert(Room { Common::ProtoHelper::getStr(room, &Protos::GUI::State::Room::name), peers, room.joined() });
   }

   int n = 0; // 'new'.
   int o = 0; // 'old'.
   while (n < roomsNewState.size() || o < this->rooms.size())
   {
      // For debugging.
      // L_DEBU(QString("n: %1, roomsNewState.size(); %2, o: %3, this->rooms.size(): %4").arg(n).arg(roomsNewState.size()).arg(o).arg(this->rooms.size()));

      // Add a new room.
      if (o == this->rooms.size() || n < roomsNewState.size() && roomsNewState.getFromIndex(n) < this->rooms.getFromIndex(o))
      {
         this->beginInsertRows(QModelIndex(), o, o);
         this->rooms.insert(roomsNewState.getFromIndex(n));
         this->endInsertRows();
      }
      // Remove a room.
      else if (n == roomsNewState.size() || o < this->rooms.size() && this->rooms.getFromIndex(o) < roomsNewState.getFromIndex(n))
      {
         this->beginRemoveRows(QModelIndex(), o, o);
         this->rooms.removeFromIndex(o--);
         this->endRemoveRows();
         n--;
      }
      // Update an existing room.
      else
      {
         Room& oldRoom = this->rooms.getFromIndex(o);
         const Room& newRoom = roomsNewState.getFromIndex(n);

         if (oldRoom.joined != newRoom.joined || oldRoom.peerIDs != newRoom.peerIDs)
         {
            oldRoom.joined = newRoom.joined;
            oldRoom.peerIDs = newRoom.peerIDs;
            emit dataChanged(this->createIndex(o, 1), this->createIndex(o, 2));
         }
      }

      if (n != roomsNewState.size())
         n++;

      if (o != this->rooms.size())
         o++;
   }
}

void RoomsModel::coreDisconnected(bool force)
{
   this->beginResetModel();
   this->rooms.clear();
   this->endResetModel();
}

bool GUI::operator<(const RoomsModel::Room& r1, const RoomsModel::Room& r2)
{
   return r1.name < r2.name;
}
