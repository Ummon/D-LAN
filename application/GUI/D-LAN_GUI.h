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
  
#ifndef GUI_D_LAN_GUI_H
#define GUI_D_LAN_GUI_H

#include <QApplication>
#include <QMenu>
#include <QSystemTrayIcon>

#include <MainWindow.h>

#include <Common/RemoteCoreController/Types.h>

namespace GUI
{
   class D_LAN_GUI : public QApplication
   {
      Q_OBJECT
   public:
      D_LAN_GUI(int argc, char *argv[]);

   private slots:
      void trayIconActivated(QSystemTrayIcon::ActivationReason reason);
      void mainWindowClosed();
      void showMainWindow();
      void exitGUI();
      void exit(bool stopTheCore = true);

   private:
      MainWindow* mainWindow;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      QSystemTrayIcon trayIcon;
      QMenu trayIconMenu;
   };
}

#endif
