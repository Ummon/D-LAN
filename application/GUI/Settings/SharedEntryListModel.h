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

#include <QString>
#include <QStringList>
#include <QAbstractTableModel>

#include <Common/SharedEntry.h>

namespace GUI
{
   class SharedEntryListModel : public QAbstractTableModel
   {
      Q_OBJECT

   public:
      enum Column
      {
         NAME = 0,
         PATH = 1,
         SIZE = 2,
         FREE_SPACE = 3,
      };

      void setEntries(const QList<Common::SharedEntry>& entries);
      void addEntries(const QStringList& entries);

      void rmEntry(int row);
      void mvUpEntry(int row);
      void mvDownEntry(int row);

      QString getLocationPath(const QModelIndex& index) const;
      const QList<Common::SharedEntry>& getSharedEntries() const;

      Common::SharedEntry getSharedEntry(const Common::Hash& ID) const;
      Common::SharedEntry getSharedDir(const Common::Hash& ID) const;
      Common::SharedEntry getSharedFile(const Common::Hash& ID) const;

      QList<Common::SharedEntry> getSharedDirectories() const;
      QList<Common::SharedEntry> getSharedFiles() const;

      int rowCount(const QModelIndex& parent = QModelIndex()) const override;
      int columnCount(const QModelIndex& parent = QModelIndex()) const override;
      QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
      Qt::ItemFlags flags(const QModelIndex &index) const override;
      bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole) override;
      QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

   public slots:
      void setEditing(const QModelIndex& index);

   signals:
      void nameChanged(const QModelIndex& index);

   private:
      QList<Common::SharedEntry> sharedEntries;
      QModelIndex currentEditingIndex;
   };
}
