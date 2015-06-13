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
  
#ifndef GUI_ROOMS_MODEL_H
#define GUI_ROOMS_MODEL_H

#include <QAbstractTableModel>
#include <QSet>
#include <QList>

#include <Protos/gui_protocol.pb.h>
#include <Protos/gui_settings.pb.h>

#include <Common/Hash.h>
#include <Common/Containers/SortedArray.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class RoomsModel : public QAbstractTableModel
   {
      Q_OBJECT
      class Room;

   public:
      RoomsModel(QSharedPointer<RCC::ICoreConnection> coreConnection);
      ~RoomsModel();

      //QModelIndex	index(int row, int column, const QModelIndex& parent = QModelIndex()) const;
      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

      QString getRoomName(const QModelIndex& index);

      void setSortType(Protos::GUI::Settings::RoomSortType sortType);
      Protos::GUI::Settings::RoomSortType getSortType() const;

   signals:
      void roomJoined(const QString& room);
      void roomLeft(const QString& room);

   private slots:
      void newState(const Protos::GUI::State& state);
      void coreDisconnected(bool force);

   private:
      void updateRooms(const google::protobuf::RepeatedPtrField<Protos::GUI::State::Room>& rooms);

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      Common::SortedArray<Room*> orderedRooms;
      QHash<QString, Room*> indexedRooms; // Rooms indexed by their name.
      Protos::GUI::Settings::RoomSortType currentSortType;
   };
}

#endif
