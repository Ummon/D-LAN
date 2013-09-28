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
  
#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>
#include <QLabel>
#include <QIcon>
#include <QMdiSubWindow>
#include <QKeyEvent>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Log/LogModel.h>
#include <Log/LogDelegate.h>

// Dockable widgets.
#include <Peers/PeersDock.h>
#include <Chat/RoomsDock.h>
#include <Search/SearchDock.h>

#include <Taskbar/Taskbar.h>
#include <MDI/MdiArea.h>

namespace Ui {
   class MainWindow;
}

namespace GUI
{
   class MainWindow : public QMainWindow
   {
      Q_OBJECT
      static const int WINDOW_BORDER_RADIUS = 10; // Only used when a custom style is selected.

   public:
      explicit MainWindow(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = nullptr);
      ~MainWindow();

   signals:
      void languageChanged(const QString& filename);

   private slots:
      void coreConnectionError(RCC::ICoreConnection::ConnectionErrorCode errorCode);
      void coreConnected();
      void coreDisconnected(bool forced);

      void browsePeer(const Common::Hash& peerID);

      void search(const QString& terms, bool ownFiles);

      void roomJoined(const QString& name);

      void logScrollChanged(int value);
      void newLogMessage();

      void loadCustomStyle(const QString& filepath);

      void maximize();

      void logEntireQWidgetTree();

   protected:
      void keyPressEvent(QKeyEvent* event);
      void closeEvent(QCloseEvent * event);
      void changeEvent(QEvent* event);

      bool eventFilter(QObject* obj, QEvent* event);

      void resizeEvent(QResizeEvent* event);

#ifdef Q_OS_WIN32
      void showEvent(QShowEvent* event);
      bool winEvent(MSG* message, long* result);
#endif

   private:
      void saveWindowsSettings();
      void restoreWindowsSettings();

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      Ui::MainWindow* ui;

      SearchDock* searchDock;
      PeersDock* peersDock;
      RoomsDock* roomsDock;

      MdiArea* mdiArea;

      QPoint dragPosition; // Used by custome styles.
      bool customStyleLoaded;
      Qt::WindowFlags initialWindowFlags;

      bool logAutoScroll;
      LogModel logModel;
      LogDelegate logDelegate;

      Taskbar taskbar;
   };
}

#endif
