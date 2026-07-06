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

#include <D-LAN_GUI.h>
using namespace GUI;

#include <QMessageBox>
#include <QPushButton>

#include <Common/LogManager/Builder.h>
#include <Common/Constants.h>
#include <Common/Settings.h>
#include <Common/Languages.h>
#include <Common/RemoteCoreController/Builder.h>

#include <Log.h>

const QString D_LAN_GUI::SHARED_MEMORY_KEYNAME("D-LAN GUI instance");

/**
  * @class GUI::D_LAN_GUI
  * This class control the trayIcon and create the main window.
  * The main window can be hid and deleted, the tray icon will still remain and will permit to relaunch the main window.
  */

D_LAN_GUI::D_LAN_GUI(int& argc, char* argv[]) :
   QApplication(argc, argv),
   mainWindow(0),
   trayIcon(QIcon(":/icons/resources/icon.svg")),
   coreConnection(RCC::Builder::newCoreConnection(SETTINGS.get<quint32>("socket_timeout")))
{
   this->installTranslator(&this->translator);
   QLocale current = QLocale::system();
   if (SETTINGS.isSet("language"))
      current = SETTINGS.get<QLocale>("language");
   Common::Languages langs(QCoreApplication::applicationDirPath() + "/" + Common::Constants::LANGUAGE_DIRECTORY);
   this->loadLanguage(langs.getBestMatchLanguage(Common::Languages::ExeType::GUI, current).filename);

   // If multiple instance isn't allowed we will test if a particular
   // shared memory segment already exists. There is actually no
   // easy way to bring the already existing GUI windows to the front without
   // dirty polling.
   // Under linux the flag may persist after process crash.
#ifndef Q_OS_LINUX
   if (!SETTINGS.get<bool>("multiple_instance_allowed"))
   {
      this->sharedMemory.lock();
      this->sharedMemory.setKey(SHARED_MEMORY_KEYNAME);
      if (!this->sharedMemory.create(1))
      {
         QMessageBox message;
         message.setWindowTitle(QObject::tr("D-LAN already launched"));
         message.setText(QObject::tr("An instance of D-LAN is already launched"));
         message.setIcon(QMessageBox::Information);
         QAbstractButton* abortButton = message.addButton(QObject::tr("Quit"), QMessageBox::RejectRole);
         message.addButton(QObject::tr("Launch anyway"), QMessageBox::ActionRole);
         message.exec();
         if (message.clickedButton() == abortButton)
         {
            this->sharedMemory.unlock();
            QSharedPointer<LM::ILogger> mainLogger = LM::Builder::newLogger("D-LAN GUI");
            mainLogger->log("User interface already launched, exiting . . .", LM::SV_END_USER);
            throw AbortException();
         }
      }
      this->sharedMemory.unlock();
   }
#endif

   this->setQuitOnLastWindowClosed(false);

   this->showMainWindow();

   RCC::ICoreConnection* coreConnectionPointer = this->coreConnection.data();
   connect(coreConnectionPointer, &RCC::ICoreConnection::localCoreStatusChanged, this, &D_LAN_GUI::updateTrayIconMenu);
   connect(coreConnectionPointer, &RCC::ICoreConnection::connected, this, &D_LAN_GUI::updateTrayIconMenu);
   connect(coreConnectionPointer, &RCC::ICoreConnection::disconnected, this, &D_LAN_GUI::updateTrayIconMenu);

   connect(&this->trayIcon, &QSystemTrayIcon::activated, this, &D_LAN_GUI::trayIconActivated);

   this->updateTrayIconMenu();

   this->trayIcon.setContextMenu(&this->trayIconMenu);
   this->trayIcon.setToolTip("D-LAN");
   this->trayIcon.show();
}

bool D_LAN_GUI::notify(QObject* receiver, QEvent* event)
{
   try
   {
      return QApplication::notify(receiver, event);
   }
   catch (const std::exception& e)
   {
      qCritical()
         << "Exception in event handler:" << e.what()
         << "| receiver:" << receiver->objectName()
         << receiver->metaObject()->className()
         << "| event type:" << event->type();
   }
   catch (...)
   {
      qCritical()
         << "Unknown exception in event handler"
         << "| receiver:" << receiver->objectName()
         << receiver->metaObject()->className()
         << "| event type:" << event->type();
   }
   // Decide policy here: swallow, or terminate cleanly.
   // std::abort();
   return false;
}

bool D_LAN_GUI::event(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->updateTrayIconMenu();

   return QApplication::event(event);
}

void D_LAN_GUI::trayIconActivated(QSystemTrayIcon::ActivationReason reason)
{
   if (reason == QSystemTrayIcon::Trigger)
      this->showMainWindow();
}

void D_LAN_GUI::updateTrayIconMenu()
{
   this->trayIconMenu.clear();
   this->trayIconMenu.addAction(tr("Show the user interface"), this, &D_LAN_GUI::showMainWindow);

   // We cannot stop a parent process without killing his child (case with RCC::RUNNING_AS_SUB_PROCESS).
   if (this->coreConnection->getLocalCoreStatus() == RCC::RUNNING_AS_SERVICE)
      this->trayIconMenu.addAction(tr("Stop the user interface"), this, &D_LAN_GUI::exitGUI);

   this->trayIconMenu.addSeparator();
   this->trayIconMenu.addAction(tr("Exit"), this, &D_LAN_GUI::exit);
}

/**
  * Load a translation file. If 'filename' is empty the default language is loaded.
  */
void D_LAN_GUI::loadLanguage(const QString& filename)
{
   const QString directory = QCoreApplication::applicationDirPath() + "/" + Common::Constants::LANGUAGE_DIRECTORY;
   if (!this->translator.load(filename, directory))
   {
      L_WARN(QString("Can't load translation file '%1' from directory '%2").arg(filename, directory));
   }
}

void D_LAN_GUI::mainWindowClosed()
{
   if (this->coreConnection->isConnected())
      // TODO: translate?
      this->trayIcon.showMessage(
         "D-LAN user interface closed",
         "D-LAN Core is still running in background. Select 'exit' from the contextual menu if you want to stop it."
      );
   this->coreConnection->disconnectFromCore();
   this->mainWindow = nullptr;
}

void D_LAN_GUI::showMainWindow()
{
   if (this->mainWindow)
   {
      this->mainWindow->setWindowState(Qt::WindowActive);
      this->mainWindow->raise();
      this->mainWindow->activateWindow();
   }
   else
   {
      this->mainWindow = new MainWindow(this->coreConnection);
      connect(this->mainWindow, &MainWindow::languageChanged, this, &D_LAN_GUI::loadLanguage);
      connect(this->mainWindow, &MainWindow::destroyed, this, &D_LAN_GUI::mainWindowClosed);
      this->mainWindow->show();
   }
}

/**
  * Stop only the GUI.
  */
void D_LAN_GUI::exitGUI()
{
   this->exit(false);
}

void D_LAN_GUI::exit(bool stopTheCore)
{
   this->trayIcon.hide();

   if (stopTheCore)
      this->coreConnection->stopLocalCore();

   if (this->mainWindow)
   {
      disconnect(this->mainWindow, &MainWindow::destroyed, this, &D_LAN_GUI::mainWindowClosed);
      delete this->mainWindow;
   }

   this->quit();
}
