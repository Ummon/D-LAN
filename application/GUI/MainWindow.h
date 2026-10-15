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
  
#pragma once

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
#include <DialogDecorator.h>

namespace Ui {
   class MainWindow;
}

namespace GUI
{
   class MainWindow : public QMainWindow
   {
      Q_OBJECT

      // The window decoration of the custom styles, a style can change it with 'qproperty-<name>' in a 'GUI--MainWindow' rule.
      // The values are reset to their default when a style is loaded.
      Q_PROPERTY(int windowBorderRadius MEMBER windowBorderRadius NOTIFY windowDecorationChanged)
      // Width of the frame around the window, the style draws it with a border. It can be dragged to resize the window.
      Q_PROPERTY(int windowFrameWidth MEMBER windowFrameWidth NOTIFY windowDecorationChanged)
      // The title bar spans the whole width of the window instead of only the central area.
      Q_PROPERTY(bool titleBarOverDocks MEMBER titleBarOverDocks NOTIFY windowDecorationChanged)
      // Margins and spacing of the title bar layout.
      Q_PROPERTY(int titleBarSpacing MEMBER titleBarSpacing NOTIFY windowDecorationChanged)
      // Width of the frame of the dialogs with a fixed size, the resizable ones have the frame of the main window (see
      // 'DialogDecorator'). 0: the dialogs keep the decoration of the system.
      Q_PROPERTY(int dialogFrameWidth MEMBER dialogFrameWidth NOTIFY windowDecorationChanged)
      // The style is drawn pixel for pixel, without the scaling of the screen. It's applied when the GUI starts (see 'main.cpp').
      Q_PROPERTY(bool ignoreScreenScaling MEMBER ignoreScreenScaling)

      static const int DEFAULT_WINDOW_BORDER_RADIUS = 10;
      static const int DEFAULT_TITLE_BAR_SPACING = 3; // As in 'MainWindow.ui'.

   public:
      explicit MainWindow(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent = nullptr);
      ~MainWindow();

   signals:
      void languageChanged(const QString& filename);
      void windowDecorationChanged();

   private slots:
      void coreConnectionError(RCC::ICoreConnection::ConnectionErrorCode errorCode);
      void coreConnected();
      void coreDisconnected(bool forced);

      void logScrollChanged(int value);
      void newLogMessage();

      void loadCustomStyle(const QString& styleName);
      void updateWindowDecoration();

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
      bool nativeEvent(const QByteArray& eventType, void* message, qintptr* result);
#endif

   private:
      void updateScreenScalingSetting(bool askForRestart);
      void updateWindowMask();
      void updateTitleBarMask();

      void saveWindowsSettings();
      void restoreWindowsSettings();

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      Ui::MainWindow* ui;

      SearchDock* searchDock;
      PeersDock* peersDock;
      RoomsDock* roomsDock;

      MdiArea* mdiArea;

      QPoint dragPosition; // Used by custom styles.
      bool customStyleLoaded;
      Qt::WindowFlags initialWindowFlags;

      int windowBorderRadius;
      int windowFrameWidth;
      bool titleBarOverDocks;
      int titleBarSpacing;
      int dialogFrameWidth;
      bool ignoreScreenScaling;
      DialogDecorator* dialogDecorator;

      bool logAutoScroll;
      LogModel logModel;
      LogDelegate logDelegate;

      Taskbar taskbar;
   };
}
