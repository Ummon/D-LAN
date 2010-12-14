#include <AybabtuGUI.h>
using namespace GUI;

#include <Common/Constants.h>

#include <CoreController.h>

/**
  * @class AybabtuGUI
  * This class control the trayIcon and create the main window.
  * The main window can be hid and deleted, the tray icon will still remain and will permit to relaunch the main window.
  */

AybabtuGUI::AybabtuGUI(int argc, char *argv[]) :
    QApplication(argc, argv), mainWindow(0), trayIcon(QIcon(":/icons/ressources/aybabtu_icon.ico"))
{
   this->setQuitOnLastWindowClosed(false);

   this->showMainWindow();

   connect(&this->trayIcon, SIGNAL(activated(QSystemTrayIcon::ActivationReason)), this, SLOT(trayIconActivated(QSystemTrayIcon::ActivationReason)));
   this->trayIconMenu.addAction("Show the GUI", this, SLOT(showMainWindow()));
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
      this->mainWindow = new MainWindow();
      connect(this->mainWindow, SIGNAL(destroyed()), this, SLOT(mainWindowClosed()));
      this->mainWindow->show();
   }
}

/**
  * Stop the Core and the GUI.
  */
void AybabtuGUI::exit()
{
   this->trayIcon.hide();

   CoreController::StopCore();

   if (this->mainWindow)
   {
      disconnect(this->mainWindow, SIGNAL(destroyed()), this, SLOT(mainWindowClosed()));
      delete this->mainWindow;
   }

   this->quit();
}
