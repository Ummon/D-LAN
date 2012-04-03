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
  
#include <Settings/DirListModel.h>
using namespace GUI;

#include <Common/Global.h>

void DirListModel::setDirs(const QList<Common::SharedDir>& dirs)
{
   // TODO: Try to factor this code with the one in BrowseModel::synchronizeRoot
   int j = 0;
   for (int i = 0; i < dirs.size(); i++)
   {
      for (int j2 = j; j2 < this->dirs.size(); j2++)
      {
         if (dirs[i] == this->dirs[j2]) // ID's are equal -> same dir.
         {
            if (!this->dirs[j2].equalTo(dirs[i])) // Data are not the same -> we refresh it.
            {
               this->dirs[j2] = dirs[i];
               emit dataChanged(this->index(j2, 0), this->index(j2, this->columnCount() - 1));
            }

            if (j2 != j)
            {
               this->beginMoveRows(QModelIndex(), j2, j2, QModelIndex(), j);
               this->dirs.move(j2, j);
               this->endMoveRows();
            }
            j++;
            goto nextEntry;
         }
      }
      this->beginInsertRows(QModelIndex(), j, j);
      this->dirs.insert(j++, dirs[i]);
      this->endInsertRows();
      nextEntry:;
   }

   while (j < this->dirs.size())
   {
      this->beginRemoveRows(QModelIndex(), j, j);
      this->dirs.removeAt(j);
      this->endRemoveRows();
   }
}

void DirListModel::addDir(const Common::SharedDir& dir)
{
   if (this->dirs.contains(dir))
      return;

   this->beginInsertRows(QModelIndex(), this->dirs.size(), this->dirs.size());
   this->dirs << dir;
   this->endInsertRows();
}

void DirListModel::addDirs(const QStringList& dirs)
{
   foreach (QString dir, dirs)
      this->addDir(Common::SharedDir(Common::Hash(), dir, 0 ,0));
}

void DirListModel::rmDir(int row)
{
   if (row >= this->dirs.size())
      return;

   this->beginRemoveRows(QModelIndex(), row, row);
   this->dirs.removeAt(row);
   this->endRemoveRows();
}

void DirListModel::mvUpDir(int row)
{
   if (row >= this->dirs.size() || row == 0)
      return;

   this->beginMoveRows(QModelIndex(), row, row, QModelIndex(), row - 1);
   this->dirs.move(row, row - 1);
   this->endMoveRows();
}

void DirListModel::mvDownDir(int row)
{
   if (row >= this->dirs.size() - 1)
      return;

   this->beginMoveRows(QModelIndex(), row, row, QModelIndex(), row + 2);
   this->dirs.move(row, row + 1);
   this->endMoveRows();
}

QString DirListModel::getLocationPath(const QModelIndex& index) const
{
   if (index.row() >= this->dirs.size())
      return QString();

   return this->dirs[index.row()].path;
}

const QList<Common::SharedDir>& DirListModel::getDirs() const
{
   return this->dirs;
}

Common::SharedDir DirListModel::getDir(const Common::Hash& ID) const
{
   for (QListIterator<Common::SharedDir> i(this->dirs); i.hasNext();)
   {
      Common::SharedDir dir = i.next();
      if (dir.ID == ID)
         return dir;
   }
   return Common::SharedDir();
}

int DirListModel::rowCount(const QModelIndex& parent) const
{
   return this->dirs.size();
}

int DirListModel::columnCount(const QModelIndex& parent) const
{
   return 3;
}

QVariant DirListModel::data(const QModelIndex& index, int role) const
{
   if (index.row() >= this->dirs.size())
      return QVariant();

   switch (role)
   {
   case Qt::DisplayRole:
      switch (index.column())
      {
      case 0:
         return this->dirs[index.row()].path;
      case 1:
         return Common::Global::formatByteSize(this->dirs[index.row()].size);
      case 2:
         return Common::Global::formatByteSize(this->dirs[index.row()].freeSpace);
      default:
         return QVariant();
      }

   case Qt::TextAlignmentRole:
      return index.column() == 0 ? Qt::AlignLeft : Qt::AlignRight;

   default: return QVariant();
   }
}

QVariant DirListModel::headerData(int section, Qt::Orientation orientation, int role) const
{
   if (orientation == Qt::Vertical)
      return QAbstractTableModel::headerData(section, orientation, role);

   switch (role)
   {
   case Qt::DisplayRole:
      switch (section)
      {
      case 0: return tr("Directory");
      case 1: return tr("Size");
      case 2: return tr("Free space");
      default: return QAbstractTableModel::headerData(section, orientation, role);
      }

   case Qt::TextAlignmentRole:
      return section == 0 ? Qt::AlignLeft : Qt::AlignRight;
   }

   return QAbstractTableModel::headerData(section, orientation, role);
}
