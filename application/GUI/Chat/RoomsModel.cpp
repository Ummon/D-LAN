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

/**
  * @class RoomsModel
  *
  * The list of all rooms.
  */

RoomsModel::RoomsModel(QSharedPointer<RCC::ICoreConnection> coreConnection) :
   coreConnection(coreConnection)
{
   connect(coreConnection.data(), SIGNAL(newState(const Protos::GUI::State&)), this, SIGNAL(newState(const Protos::GUI::State&)));
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

   //QMap<QString, Room*>::const_iterator i = this->rooms.begin() + 10;

   switch (role)
   {
   case Qt::DisplayRole:
      switch (index.column())
      {
      case 0: return this->rooms.getFromIndex(index.row()).name;
      case 1: return this->rooms.getFromIndex(index.row()).peerIDs.size();
      }
      break;

   case Qt::TextAlignmentRole:
      return (index.column() == 1 ? Qt::AlignRight : Qt::AlignLeft) + Qt::AlignVCenter;
   }

   return QVariant();
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

   // Merge 'roomsNewState' in 'this->rooms'.
   // <TODO>
}

bool GUI::operator<(const RoomsModel::Room& r1, const RoomsModel::Room& r2)
{
   return r1.name < r2.name;
}

bool GUI::operator==(const RoomsModel::Room& r1, const RoomsModel::Room& r2)
{
   return r1.name == r2.name;
}
