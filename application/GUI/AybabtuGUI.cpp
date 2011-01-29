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
  
#include <AybabtuGUI.h>
using namespace GUI;

#include <Common/Constants.h>

#include <Common/RemoteCoreController/Builder.h>

/**
  * @class AybabtuGUI
  * This class control the trayIcon and create the main window.
  * The main window can be hid and deleted, the tray icon will still remain and will permit to relaunch the main window.
  */

AybabtuGUI::AybabtuGUI(int argc, char *argv[]) :
   QApplication(argc, argv),
   mainWindow(0),
   coreConnection(RCC::Builder::newCoreConnection()),
   trayIcon(QIcon(":/icons/ressources/aybabtu_icon.png"))
{
   this->setQuitOnLastWindowClosed(false);

   this->showMainWindow();

   connect(&this->trayIcon, SIGNAL(activated(QSystemTrayIcon::ActivationReason)), this, SLOT(trayIconActivated(QSystemTrayIcon::ActivationReason)));
   this->trayIconMenu.addAction("Show the GUI", this, SLOT(showMainWindow()));
   if (!this->coreConnection->isRunningAsSubProcess()) // We cannot stop a parent process without killing his child.
      this->trayIconMenu.addAction("Stop the GUI", this, SLOT(exitGUI()));
   this->trayIconMenu.addSeparator();
   this->trayIconMenu.addAction("Exit", this, SLOT(exit()));
   this->trayIcon.setContextMenu(&this->trayIconMenu);
   this->trayIcon.setToolTip("Aybabtu");
   this->trayIcon.show();
}

void AybabtuGUI::trayIconActivated(QSystemTrayIcon::ActivationReason reason)
{
   if (reason == QSystemTrayIcon::Trigger)
      this->showMainWindow();
}

void AybabtuGUI::mainWindowClosed()
{
   this->trayIcon.showMessage("Aybabtu GUI closed", "Aybabtu Core is still running in background. Select 'exit' from the contextual menu if you want to stop it.");
   this->mainWindow = 0;
}

void AybabtuGUI::showMainWindow()
{
   if (this->mainWindow)
   {
      this->mainWindow->setWindowState(Qt::WindowActive);
      this->mainWindow->raise();
   }
   else
   {
      this->mainWindow = new MainWindow(this->coreConnection);
      connect(this->mainWindow, SIGNAL(destroyed()), this, SLOT(mainWindowClosed()));
      this->mainWindow->show();
   }
}

/**
  * Stop only the GUI.
  */
void AybabtuGUI::exitGUI()
{
   this->exit(false);
}

void AybabtuGUI::exit(bool stopTheCore)
{
   this->trayIcon.hide();

   if (stopTheCore)
      RCC::Builder::StopCore();

   if (this->mainWindow)
   {
      disconnect(this->mainWindow, SIGNAL(destroyed()), this, SLOT(mainWindowClosed()));
      delete this->mainWindow;
   }

   this->quit();
}
