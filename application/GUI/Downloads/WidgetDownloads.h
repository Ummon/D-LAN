/**
  * Aybabtu - A decentralized LAN file sharing software.
  * Copyright (C) 2010-2011 Greg Burri <greg.burri@gmail.com>
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
  
#ifndef GUI_WIDGETDOWNLOADS_H
#define GUI_WIDGETDOWNLOADS_H

#include <QWidget>
#include <QPoint>
#include <QStyledItemDelegate>

#include <CheckBoxList.h>
#include <CheckBoxModel.h>
#include <PeerList/PeerListModel.h>
#include <CoreConnection/CoreConnection.h>
#include <Downloads/DownloadFilterStatus.h>
#include <Downloads/DownloadsModel.h>

namespace Ui {
   class WidgetDownloads;
}

namespace GUI
{
   class DownloadsDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class WidgetDownloads : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetDownloads(CoreConnection& coreConnection, PeerListModel& peerListModel, QWidget *parent = 0);
      ~WidgetDownloads();

   private slots:
      void displayContextMenuDownloads(const QPoint& point);
      void openLocationSelectedEntries();
      void removeSelectedEntries();
      void removeCompletedFiles();
      void filterChanged();

   private:
      Ui::WidgetDownloads *ui;
      CheckBoxList* filterStatusList;

      CoreConnection& coreConnection;

      CheckBoxModel<DownloadFilterStatus> checkBoxModel;
      DownloadsModel downloadsModel;
      DownloadsDelegate downloadsDelegate;
   };
}

#endif
