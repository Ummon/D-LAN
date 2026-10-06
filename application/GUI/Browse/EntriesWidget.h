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

#include <QWidget>
#include <QTreeView>
#include <QModelIndex>
#include <QSharedPointer>

#include <Common/Hash.h>
#include <Common/Path.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Browse/BrowseModel.h>

namespace GUI
{
   class EntriesWidget : public QWidget
   {
      Q_OBJECT
   public:
      EntriesWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = nullptr);

   protected:
      void setEntries(const BrowseModel* model, const QTreeView* view);

      /**
        * The peer owning the entry at the given index.
        */
      virtual Common::Hash entryPeerID(const QModelIndex& index) const = 0;

      virtual bool hasOwnLocation(const QModelIndex& index) const;

      void openFile(const QModelIndex& index) const;

      void keyPressEvent(QKeyEvent* event) override;

   protected slots:
      void download();

      void downloadTo();
      void downloadTo(const Common::Path& path);
      void downloadTo(const Common::Hash& sharedDirID, const Common::Path& relativePath);

      void openLocation();

   protected:
      QSharedPointer<RCC::ICoreConnection> coreConnection;

   private:
      QModelIndexList selectedRows() const;

      const BrowseModel* model = nullptr;
      const QTreeView* view = nullptr;
   };
}
