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
  
#ifndef GUI_AYBABTUGUI_H
#define GUI_AYBABTUGUI_H

#include <QApplication>
#include <QMenu>
#include <QSystemTrayIcon>

#include <MainWindow.h>

namespace GUI
{
   class AybabtuGUI : public QApplication
   {
      Q_OBJECT
   public:
      AybabtuGUI(int argc, char *argv[]);

   private slots:
      void trayIconActivated(QSystemTrayIcon::ActivationReason reason);
      void mainWindowClosed();
      void showMainWindow();
      void exit();

   private:
      MainWindow* mainWindow;

      QSystemTrayIcon trayIcon;
      QMenu trayIconMenu;
   };
}

#endif
