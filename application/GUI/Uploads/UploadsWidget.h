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
  
#ifndef GUI_UPLOADSWINDOW_H
#define GUI_UPLOADSWINDOW_H

#include <QWidget>
#include <QProgressBar>
#include <QStyledItemDelegate>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Uploads/UploadsModel.h>
#include <Peers/PeerListModel.h>

namespace Ui {
   class UploadsWidget;
}

namespace GUI
{
   class UploadsDelegate : public QStyledItemDelegate
   {
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
      QSize sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const;
      void drawFocus(QPainter*, const QStyleOptionViewItem&, const QRect&) const {}

   private:
      QProgressBar model;
   };

   class UploadsWidget : public QWidget
   {
      Q_OBJECT
   public:
      explicit UploadsWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, PeerListModel& peerListModel, QWidget* parent = nullptr);
      ~UploadsWidget();

   protected:
      void changeEvent(QEvent* event);

   private:
      Ui::UploadsWidget* ui;

      UploadsModel uploadsModel;
      UploadsDelegate uploadsDelegate;
   };
}

#endif
