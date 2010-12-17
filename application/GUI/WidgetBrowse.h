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
  
#ifndef GUI_WIDGETBROWSE_H
#define GUI_WIDGETBROWSE_H

#include <QWidget>
#include <QStyledItemDelegate>

#include <Common/Hash.h>

#include <PeerListModel.h>
#include <CoreConnection.h>
#include <BrowseModel.h>

namespace Ui {
   class WidgetBrowse;
}

namespace GUI
{
   class BrowseDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class WidgetBrowse : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetBrowse(CoreConnection& coreConnection, PeerListModel& peerListModel, const Common::Hash& peerID, QWidget *parent = 0);
      ~WidgetBrowse();
      Common::Hash getPeerID() const;

   private slots:
      void displayContextMenuPeers(const QPoint& point);
      void download();

   private:
      Ui::WidgetBrowse* ui;

      CoreConnection& coreConnection;
      PeerListModel& peerListModel;
      const Common::Hash peerID;

      BrowseModel browseModel;
      BrowseDelegate browseDelegate;
   };
}
#endif
