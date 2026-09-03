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

#include <Settings/SharedEntryListModel.h>
using namespace GUI;

#include <algorithm>

#include <QFileInfo>
#include <QDir>

#include <Common/Global.h>

#include <IconProvider.h>

void SharedEntryListModel::setEntries(const QList<Common::SharedEntry>& entries)
{
   // Model updating is not permitted during editing.
   if (this->currentEditingIndex.isValid())
      return;

   // TODO: Try to factor this code with the one in BrowseModel::synchronizeRoot
   int j = 0;
   for (int i = 0; i < entries.size(); i++)
   {
      for (int j2 = j; j2 < this->sharedEntries.size(); j2++)
      {
         if (entries[i] == this->sharedEntries[j2]) // ID's are equal -> same entry.
         {
            if (!this->sharedEntries[j2].equalTo(entries[i])) // Data are not the same -> we refresh it.
            {
               this->sharedEntries[j2] = entries[i];
               emit dataChanged(this->index(j2, 0), this->index(j2, this->columnCount() - 1));
            }

            if (j2 != j)
            {
               this->beginMoveRows(QModelIndex(), j2, j2, QModelIndex(), j);
               this->sharedEntries.move(j2, j);
               this->endMoveRows();
            }
            j++;
            goto nextEntry;
         }
      }
      this->beginInsertRows(QModelIndex(), j, j);
      this->sharedEntries.insert(j++, entries[i]);
      this->endInsertRows();
      nextEntry:;
   }

   while (j < this->sharedEntries.size())
   {
      this->beginRemoveRows(QModelIndex(), j, j);
      this->sharedEntries.removeAt(j);
      this->endRemoveRows();
   }
}

void SharedEntryListModel::addEntries(const QStringList& entries)
{
   QList<Common::SharedEntry> sharedEntries;

   for (const auto& entry : entries)
   {
      QString cleaned = QDir::cleanPath(entry);
      if (QFileInfo(cleaned).isDir() && (cleaned.isEmpty() || !cleaned.endsWith('/')))
         cleaned += '/';

      const auto sharedEntry = Common::SharedEntry { Common::Hash(), cleaned, QString(), 0, 0 };

      // The paths are compared and not the entries themselves: 'SharedEntry::operator==', which 'contains(..)'
      // would use, only compares the IDs and a new entry doesn't have one yet, the core assigns it. It would
      // both let an already shared entry be added again and, as soon as one entry is waiting for its ID, make
      // every following one look like a duplicate.
      const bool alreadyShared =
         std::any_of(
            this->sharedEntries.constBegin(),
            this->sharedEntries.constEnd(),
            [&sharedEntry](const Common::SharedEntry& e) { return e.path == sharedEntry.path; }
         );

      if (!alreadyShared)
         sharedEntries << sharedEntry;
   }

   if (sharedEntries.isEmpty())
      return;

   this->beginInsertRows(QModelIndex(), this->sharedEntries.size(), this->sharedEntries.size() + sharedEntries.size() - 1);
   this->sharedEntries.append(sharedEntries);
   this->endInsertRows();
}

void SharedEntryListModel::rmEntry(int row)
{
   if (row >= this->sharedEntries.size())
      return;

   this->beginRemoveRows(QModelIndex(), row, row);
   this->sharedEntries.removeAt(row);
   this->endRemoveRows();
}

void SharedEntryListModel::mvUpEntry(int row)
{
   if (row >= this->sharedEntries.size() || row == 0)
      return;

   this->beginMoveRows(QModelIndex(), row, row, QModelIndex(), row - 1);
   this->sharedEntries.move(row, row - 1);
   this->endMoveRows();
}

void SharedEntryListModel::mvDownEntry(int row)
{
   if (row >= this->sharedEntries.size() - 1)
      return;

   this->beginMoveRows(QModelIndex(), row, row, QModelIndex(), row + 2);
   this->sharedEntries.move(row, row + 1);
   this->endMoveRows();
}

QString SharedEntryListModel::getLocationPath(const QModelIndex& index) const
{
   if (index.row() >= this->sharedEntries.size())
      return QString();

   return this->sharedEntries[index.row()].path.toString();
}

const QList<Common::SharedEntry>& SharedEntryListModel::getSharedEntries() const
{
   return this->sharedEntries;
}

/**
  * If not found return a 'null' shared entry.
  */
Common::SharedEntry SharedEntryListModel::getSharedEntry(const Common::Hash& ID) const
{
   for (QListIterator<Common::SharedEntry> i(this->sharedEntries); i.hasNext();)
   {
      const Common::SharedEntry& entry = i.next();
      if (entry.ID == ID)
         return entry;
   }
   return Common::SharedEntry();
}

Common::SharedEntry SharedEntryListModel::getSharedDir(const Common::Hash& ID) const
{
   const Common::SharedEntry& sharedEntry = this->getSharedEntry(ID);
   if (sharedEntry.isNull() || !sharedEntry.path.isFile())
      return sharedEntry;
   else
      return Common::SharedEntry();
}

Common::SharedEntry SharedEntryListModel::getSharedFile(const Common::Hash& ID) const
{
   const Common::SharedEntry& sharedEntry = this->getSharedEntry(ID);
   if (sharedEntry.isNull() || sharedEntry.path.isFile())
      return sharedEntry;
   else
      return Common::SharedEntry();
}

QList<Common::SharedEntry> SharedEntryListModel::getSharedDirectories() const
{
   QList sharedDirectories = this->sharedEntries;
   sharedDirectories.removeIf([](const auto& entry){ return entry.path.isFile(); });
   return sharedDirectories;
}

QList<Common::SharedEntry> SharedEntryListModel::getSharedFiles() const
{
   QList sharedFiles = this->sharedEntries;
   sharedFiles.removeIf([](const auto& entry){ return !entry.path.isFile(); });
   return sharedFiles;
}

int SharedEntryListModel::rowCount(const QModelIndex&) const
{
   return this->sharedEntries.size();
}

int SharedEntryListModel::columnCount(const QModelIndex&) const
{
   return 4;
}

QVariant SharedEntryListModel::data(const QModelIndex& index, int role) const
{
   if (index.row() >= this->sharedEntries.size())
      return QVariant();

   switch (role)
   {
   case Qt::EditRole:
   case Qt::DisplayRole:
      switch (index.column())
      {
      case Column::NAME:
         return this->sharedEntries[index.row()].getName();

      case Column::PATH:
         return this->sharedEntries[index.row()].path.toString();

      case Column::SIZE:
         return Common::Global::formatByteSize(this->sharedEntries[index.row()].size);

      case Column::FREE_SPACE:
         return Common::Global::formatByteSize(this->sharedEntries[index.row()].freeSpace);

      default:
         return QVariant();
      }

   case Qt::DecorationRole:
      // Icon: file or directory.
      if (index.column() == 0)
         return IconProvider::getIcon(this->sharedEntries[index.row()].path);

      return QVariant();

   case Qt::TextAlignmentRole:
      return QVariant((index.column() == 0 || index.column() == 1 ? Qt::AlignLeft : Qt::AlignRight) | Qt::AlignVCenter);

   default: return QVariant();
   }
}

Qt::ItemFlags SharedEntryListModel::flags(const QModelIndex &index) const
{
   auto flags = QAbstractTableModel::flags(index);

   if (index.column() == Column::NAME)
      return flags | Qt::ItemIsEditable;

   return flags;
}

bool SharedEntryListModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
   if (!QAbstractTableModel::setData(index, value, role))
   {
      if (role == Qt::EditRole && index.column() == Column::NAME && index.row() < this->sharedEntries.size())
      {
         const QString name = value.toString().trimmed();
         if (this->sharedEntries[index.row()].name == name)
            return false;

         this->sharedEntries[index.row()].name = name;

         emit nameChanged(index);
         emit dataChanged(index, index);
      }
   }

   return true;
}

QVariant SharedEntryListModel::headerData(int section, Qt::Orientation orientation, int role) const
{
   if (orientation == Qt::Vertical)
      return QAbstractTableModel::headerData(section, orientation, role);

   switch (role)
   {
   case Qt::DisplayRole:
      switch (section)
      {
      case 0: return tr("Name");
      case 1: return tr("Path");
      case 2: return tr("Size");
      case 3: return tr("Free space");
      default: return QAbstractTableModel::headerData(section, orientation, role);
      }

   case Qt::TextAlignmentRole:
      return section == 0 || section == 1 ? Qt::AlignLeft : Qt::AlignRight;
   }

   return QAbstractTableModel::headerData(section, orientation, role);
}

void SharedEntryListModel::setEditing(const QModelIndex& index)
{
   this->currentEditingIndex = index;
}
