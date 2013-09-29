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
  
#ifndef GUI_ROOMSDOCK_H
#define GUI_ROOMSDOCK_H

#include <QDockWidget>
#include <QSharedPointer>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Chat/RoomsModel.h>
#include <Chat/RoomsDelegate.h>

namespace Ui {
   class RoomsDock;
}

namespace GUI
{
   class RoomsDock : public QDockWidget
   {
      Q_OBJECT

   public:
      explicit RoomsDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = 0);
      ~RoomsDock();

   signals:
      void roomJoined(const QString&);

   protected:
      bool eventFilter(QObject* obj, QEvent* event);

   private slots:
      void displayContextMenuRooms(const QPoint& point);
      void roomDoubleClicked(const QModelIndex& index);
      void joinSelectedRoom();
      void joinRoom();

      void coreConnected();
      void coreDisconnected(bool force);

   private:
      void joinRoom(const QString& roomName);

      Ui::RoomsDock* ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      RoomsModel roomsModel;
      RoomsDelegate roomsDelegate;
   };
}

#endif
