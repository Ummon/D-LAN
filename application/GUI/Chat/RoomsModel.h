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

#include <Common/Hash.h>
#include <Common/SortedArray.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class RoomsModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      RoomsModel(QSharedPointer<RCC::ICoreConnection> coreConnection);

      //QModelIndex	index(int row, int column, const QModelIndex& parent = QModelIndex()) const;
      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

      QString getRoomName(const QModelIndex& index);

   signals:
      void roomJoined(const QString& room);
      void roomLeft(const QString& room);

   private slots:
      void newState(const Protos::GUI::State& state);
      void coreDisconnected(bool force);

   private:
      QSharedPointer<RCC::ICoreConnection> coreConnection;

      struct Room
      {
         QString name;
         QSet<Common::Hash> peerIDs;
         bool joined;
      };

      friend bool operator<(const Room& r1, const Room& r2);

      Common::SortedArray<Room> rooms;
   };

   bool operator<(const RoomsModel::Room& r1, const RoomsModel::Room& r2);
}

#endif
