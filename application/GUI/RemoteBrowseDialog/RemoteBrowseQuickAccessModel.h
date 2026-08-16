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

#pragma once

#include <QAbstractTableModel>
#include <QVariant>

#include <Protos/gui_protocol.pb.h>

#include <Common/ProtoHelper.h>
#include <Common/RemoteCoreController/ILocalBrowseQuickAccessResult.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

namespace GUI
{
   class RemoteBrowseQuickAccessModel : public QAbstractTableModel
   {
      Q_OBJECT

   public:
      RemoteBrowseQuickAccessModel(QSharedPointer<RCC::ICoreConnection> coreConnection);
      virtual ~RemoteBrowseQuickAccessModel();

      virtual int rowCount(const QModelIndex& parent = QModelIndex()) const override;
      virtual int columnCount(const QModelIndex& parent = QModelIndex()) const override;
      virtual QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;

   private slots:
      void result(const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseQuickAccessResult::QuickAccess>& entries);
      void resultTimeout();

   private:
      void refresh();

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      QSharedPointer<RCC::ILocalBrowseQuickAccessResult> localBrowseQuickAccessResult;

      QList<Protos::GUI::LocalBrowseQuickAccessResult::QuickAccess> quickAccess;
   };
}
