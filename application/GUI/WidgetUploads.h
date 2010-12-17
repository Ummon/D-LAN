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
  
#ifndef GUI_WIDGETUPLOADS_H
#define GUI_WIDGETUPLOADS_H

#include <QWidget>
#include <QStyledItemDelegate>

#include <UploadsModel.h>
#include <PeerListModel.h>
#include <CoreConnection.h>

namespace Ui {
   class WidgetUploads;
}

namespace GUI
{
   class UploadsDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
      void drawFocus(QPainter*, const QStyleOptionViewItem&, const QRect&) const {}
   };

   class WidgetUploads : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetUploads(CoreConnection& coreConnection, PeerListModel& peerListModel, QWidget *parent = 0);
      ~WidgetUploads();

   private:
      Ui::WidgetUploads* ui;

      UploadsModel uploadsModel;
      UploadsDelegate uploadsDelegate;
   };
}

#endif
