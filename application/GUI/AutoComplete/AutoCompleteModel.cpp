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
  
#include <AutoComplete/AutoCompleteModel.h>
using namespace GUI;

AutoCompleteModel::AutoCompleteModel()
{
}

AutoCompleteModel::~AutoCompleteModel()
{

}

void AutoCompleteModel::setValues(const QList<QPair<Common::Hash, QString>>& values)
{
   this->beginResetModel();
   this->values = values;
   this->endResetModel();
}

Common::Hash AutoCompleteModel::getHash(const QModelIndex& index) const
{
   if (index.row() >= 0 && index.row() < this->values.size())
      return this->values[index.row()].first;

   return Common::Hash();
}

/*QModelIndex AutoCompleteModel::index(int row, int column, const QModelIndex& parent) const
{
   return QModelIndex();
}

QModelIndex AutoCompleteModel::parent(const QModelIndex& child) const
{
   return QModelIndex();
}*/

int AutoCompleteModel::rowCount(const QModelIndex& parent) const
{
   return this->values.count();
}

int AutoCompleteModel::columnCount(const QModelIndex& parent) const
{
   return 1;
}

QVariant AutoCompleteModel::data(const QModelIndex& index, int role) const
{
   if (!index.isValid() || index.row() >= this->values.count())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      return this->values[index.row()].second;

   default:
      return QVariant();
   }
}
