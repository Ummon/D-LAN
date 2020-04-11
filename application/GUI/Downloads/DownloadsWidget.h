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
#include <QPoint>
#include <QPair>
#include <QStyledItemDelegate>
#include <QProgressBar>

#include <Protos/gui_settings.pb.h>

#include <Common/RemoteCoreController/ICoreConnection.h>
#include <Common/Containers/Tree.h>

#include <CheckBoxList.h>
#include <CheckBoxModel.h>
#include <Peers/PeerListModel.h>
#include <Downloads/DownloadFilterStatus.h>
#include <Downloads/DownloadsFlatModel.h>
#include <Downloads/DownloadsTreeModel.h>
#include <Settings/SharedEntryListModel.h>

namespace Ui {
   class DownloadsWidget;
}

namespace GUI
{
   class DownloadsDelegate : public QStyledItemDelegate
   {
      Q_OBJECT

   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;

   private:
      QProgressBar model;
   };

   class DownloadsWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit DownloadsWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const SharedEntryListModel& sharedEntryListModel, QWidget* parent = nullptr);
      ~DownloadsWidget() override;

   signals:
      void globalProgressChanged(quint64 completed, quint64 total);

   protected:
      void changeEvent(QEvent* event) override;
      void keyPressEvent(QKeyEvent* event) override;

   private slots:
      void displayContextMenuDownloads(const QPoint& point);
      void downloadDoubleClicked(const QModelIndex& index);
      void openLocationSelectedEntries();
      void moveSelectedEntriesToTop();
      void switchView();
      void removeCompletedFiles();
      void removeSelectedEntries();
      void pauseSelectedEntries();
      void filterChanged();
      void updateGlobalProgressBar();

   private:
      void switchView(Protos::GUI::Settings::DownloadView view);
      void updateCheckBoxElements();
      QPair<QList<quint64>, bool> getDownloadIDsToPause() const;

      void saveTreeViewState();
      void saveTreeViewState(const QModelIndex& index, Common::SimpleTree<quint32>* tree);
      void restoreTreeViewState();
      void restoreTreeViewState(const QModelIndex& index, Common::SimpleTree<quint32>* tree);

      void openFile(const QModelIndex& index) const;

      Ui::DownloadsWidget* ui;
      CheckBoxList* filterStatusList;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      CheckBoxModel<DownloadFilterStatus> checkBoxModel;

      DownloadsFlatModel downloadsFlatModel;
      DownloadsTreeModel downloadsTreeModel;
      DownloadsModel* currentDownloadsModel;

      DownloadsDelegate downloadsDelegate;

      Common::SimpleTree<quint32> treeViewState;
   };
}
