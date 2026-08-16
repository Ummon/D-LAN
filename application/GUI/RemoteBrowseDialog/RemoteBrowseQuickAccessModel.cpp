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

#include <RemoteBrowseDialog/RemoteBrowseQuickAccessModel.h>
using namespace GUI;

#include <Common/Global.h>

#include <Log.h>

RemoteBrowseQuickAccessModel::RemoteBrowseQuickAccessModel(QSharedPointer<RCC::ICoreConnection> coreConnection) :
   coreConnection(coreConnection)
{
   this->refresh();
}

RemoteBrowseQuickAccessModel::~RemoteBrowseQuickAccessModel()
{
}

int RemoteBrowseQuickAccessModel::rowCount(const QModelIndex& parent) const
{
   return this->quickAccess.count();
}

int RemoteBrowseQuickAccessModel::columnCount(const QModelIndex& parent) const
{
   return 1;
}

QVariant RemoteBrowseQuickAccessModel::data(const QModelIndex& index, int role) const
{
   if (index.row() >= this->quickAccess.size())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      return QString::fromStdString(this->quickAccess[index.row()].name());

   // case Qt::DecorationRole:
   //    // Icon: file or directory.
   //    if (index.column() == 0)
   //       return IconProvider::getIcon(this->sharedEntries[index.row()].path);

   //    return QVariant();

   // case Qt::TextAlignmentRole:
   //    return QVariant((index.column() == 0 || index.column() == 1 ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter);

   default: return QVariant();
   }
}

void RemoteBrowseQuickAccessModel::result(const google::protobuf::RepeatedPtrField<Protos::GUI::LocalBrowseQuickAccessResult::QuickAccess>& entries)
{
   if (!this->quickAccess.isEmpty())
   {
      this->beginRemoveRows(QModelIndex(), 0, this->quickAccess.length() - 1);
      this->quickAccess.clear();
      this->endRemoveRows();
   }

   if (!entries.empty())
   {
      this->beginInsertRows(QModelIndex(), 0, entries.size() - 1);
      for (const auto& entry : entries)
         this->quickAccess << entry;
      this->endInsertRows();
   }

   this->localBrowseQuickAccessResult.clear();
}

void RemoteBrowseQuickAccessModel::resultTimeout()
{
   L_WARN("Asking for local entries message timed out (RemoteBrowseQuickAccessModel)");
   this->localBrowseQuickAccessResult.clear();
}

void RemoteBrowseQuickAccessModel::refresh()
{
   if (!this->localBrowseQuickAccessResult.isNull())
      this->localBrowseQuickAccessResult->disconnect();

   this->localBrowseQuickAccessResult = this->coreConnection->localBrowseQuickAccess();
   connect(this->localBrowseQuickAccessResult.data(), &RCC::ILocalBrowseQuickAccessResult::result, this, &RemoteBrowseQuickAccessModel::result);
   connect(this->localBrowseQuickAccessResult.data(), &Common::Timeoutable::timeout, this, &RemoteBrowseQuickAccessModel::resultTimeout);
   this->localBrowseQuickAccessResult->start();
}
