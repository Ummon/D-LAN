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
  
#ifndef GUI_BROWSEWINDOW_H
#define GUI_BROWSEWINDOW_H

#include <QWidget>
#include <QStringList>
#include <QAbstractButton>
#include <QStyledItemDelegate>
#include <QItemSelection>

#include <Common/Hash.h>
#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Peers/PeerListModel.h>
#include <Settings/DirListModel.h>
#include <Browse/BrowseModel.h>
#include <DownloadMenu.h>

namespace Ui {
   class BrowseWidget;
}

namespace GUI
{
   class BrowseDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class BrowseWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit BrowseWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const Common::Hash& peerID, QWidget* parent = nullptr);
      ~BrowseWidget();
      Common::Hash getPeerID() const;
      void browseTo(const Protos::Common::Entry& remoteEntry);

   public slots:
      void refresh();

   protected:
      void changeEvent(QEvent* event);
      void keyPressEvent(QKeyEvent* event);

   private slots:
      void displayContextMenuDownload(const QPoint& point);
      void entryDoubleClicked(const QModelIndex& index);

      void download();
      void downloadTo();
      void downloadTo(const QString& path, const Common::Hash& sharedDirID = Common::Hash());

      void openLocation();
      void tryToReachEntryToBrowse();

   private:
      void openFile(const QModelIndex& index) const;

      Ui::BrowseWidget* ui;
      DownloadMenu downloadMenu;

      QSharedPointer<RCC::ICoreConnection> coreConnection;
      const Common::Hash peerID;

      BrowseModel browseModel;
      BrowseDelegate browseDelegate;

      bool tryingToReachEntryToBrowse;
      Protos::Common::Entry remoteEntryToBrowse;
   };
}
#endif
