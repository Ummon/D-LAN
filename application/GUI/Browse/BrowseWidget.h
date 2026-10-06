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
#include <QStringList>
#include <QAbstractButton>
#include <QItemSelection>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Peers/PeerListModel.h>
#include <Settings/SharedEntryListModel.h>
#include <Browse/BrowseModel.h>
#include <Browse/EntriesWidget.h>
#include <DownloadMenu.h>
#include <NoFocusDelegate.h>

namespace Ui {
   class BrowseWidget;
}

namespace GUI
{
   class BrowseWidget : public EntriesWidget
   {
      Q_OBJECT
   public:
      explicit BrowseWidget(
         QSharedPointer<RCC::ICoreConnection> coreConnection,
         const PeerListModel& peerListModel,
         const SharedEntryListModel& sharedEntryListModel,
         const Common::Hash& peerID,
         QWidget* parent = nullptr
      );
      ~BrowseWidget();
      Common::Hash getPeerID() const;
      void browseTo(const Protos::Common::Entry& remoteEntry);

   public slots:
      void refresh();

   protected:
      void changeEvent(QEvent* event) override;
      Common::Hash entryPeerID(const QModelIndex& index) const override;

   private slots:
      void displayContextMenuDownload(const QPoint& point);
      void entryDoubleClicked(const QModelIndex& index);
      void tryToReachEntryToBrowse();

   private:
      Ui::BrowseWidget* ui;
      DownloadMenu downloadMenu;

      const Common::Hash peerID;

      BrowseModel browseModel;
      NoFocusDelegate browseDelegate;

      bool tryingToReachEntryToBrowse;
      Protos::Common::Entry remoteEntryToBrowse;
   };
}
