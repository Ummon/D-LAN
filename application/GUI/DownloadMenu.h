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

#include <QObject>
#include <QSharedPointer>
#include <QStringList>
#include <QPoint>
#include <QMenu>
#include <QList>

#include <Protos/common.pb.h>

#include <Common/Hash.h>
#include <Common/Path.h>
#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/RemoteCoreController/IBrowseResult.h>

#include <Settings/SharedEntryListModel.h>

namespace GUI
{
   class DownloadMenu : public QObject
   {
      Q_OBJECT
   public:
      DownloadMenu(QSharedPointer<RCC::ICoreConnection> coreConnection, const SharedEntryListModel& sharedEntryListModel);
      void show(const QPoint& globalPosition);

   signals:
      /**
        * Download the selected items to the first available shared directory.
        */
      void download();

      /**
        * Download the selected items to a chosen custom directory.
        */
      void downloadTo();

      /**
        * Download the selected items to a folder of a shared directory.
        * @param relativePath The folder relative to the shared directory, empty for the shared directory itself.
        */
      void downloadTo(const Common::Hash& sharedDirID, const Common::Path& relativePath);

   private:
      virtual void onShowMenu(QMenu&) {}

      QMenu* createFolderMenu(
         const QString& title,
         const Common::Hash& sharedDirID,
         const QStringList& relativeDirs,
         const Protos::Common::Entry& folder,
         QWidget* parent
      );
      void loadSubFolders(QMenu* menu, QAction* actionLoading, const Common::Hash& sharedDirID, const QStringList& relativeDirs, const Protos::Common::Entry& folder);

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      const SharedEntryListModel& sharedEntryListModel;

      QList<QSharedPointer<RCC::IBrowseResult>> browseResults; // Pending browses of the currently shown menu.
   };
}
