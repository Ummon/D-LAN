/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#ifndef GUI_UPLOADSMODEL_H
#define GUI_UPLOADSMODEL_H

#include <QAbstractTableModel>

#include <Protos/gui_protocol.pb.h>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <PeerList/PeerListModel.h>

namespace GUI
{
   class UploadsModel : public QAbstractTableModel
   {
      Q_OBJECT
   public:
      UploadsModel(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel);

      int rowCount(const QModelIndex& parent = QModelIndex()) const;
      int columnCount(const QModelIndex& parent = QModelIndex()) const;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const;

   private slots:
      void newState(const Protos::GUI::State& state);

   private:
      QSharedPointer<RCC::ICoreConnection> coreConnection;
      PeerListModel& peerListModel;

      QList<Protos::GUI::State_Upload> uploads;
   };

   bool operator==(const Protos::GUI::State_Upload& u1, const Protos::GUI::State_Upload& u2);
   bool operator!=(const Protos::GUI::State_Upload& u1, const Protos::GUI::State_Upload& u2);
}

#endif
