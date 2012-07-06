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

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <PeerList/PeerListModel.h>
#include <PeerList/PeerListDelegate.h>
#include <Log/LogModel.h>
#include <Log/LogDelegate.h>

#include <Settings/WidgetSettings.h>
#include <Settings/DirListModel.h>
#include <Chat/WidgetChat.h>
#include <Downloads/WidgetDownloads.h>
#include <Uploads/WidgetUploads.h>
#include <Browse/WidgetBrowse.h>
#include <Search/WidgetSearch.h>
#include <Taskbar/Taskbar.h>
#include <BusyIndicator.h>


Q_DECLARE_METATYPE(QHostAddress)

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
      void newState(const Protos::GUI::State& state);
      void onGlobalProgressChanged(quint64 completed, quint64 total);

      void coreConnectionError(RCC::ICoreConnection::ConnectionErrorCode errorCode);
      void coreConnected();
      void coreDisconnected(bool forced);

      void tabMoved(int from, int to);

      void displayContextMenuPeers(const QPoint& point);
      void browse();
      void takeControlOfACore();
      void searchOtherPeers();
      void searchOwnFiles();

      void sortPeersBySharingAmount();
      void sortPeersByNick();
      void colorizeSelectedPeer();
      void uncolorizeSelectedPeer();

      void removeWidget(QWidget* widget);

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
      void search(bool ownFiles = false);

      void setApplicationStateAsConnected();
      void setApplicationStateAsDisconnected();

      void saveWindowsSettings();
      void restoreWindowsSettings();

      void restoreColorizedPeers();

      QString getBusyIndicatorToolTip() const;

      void removeMdiSubWindow(QMdiSubWindow* mdiSubWindow);

      void addWidgetSettings();
      void removeWidgetSettings();

      void addWidgetChat();      
      void removeWidgetChat();

      void addWidgetDownloads();
      void removeWidgetDownloads();

      void addWidgetUploads();
      void removeWidgetUploads();

      WidgetBrowse* addWidgetBrowse(const Common::Hash& peerID);
      private slots: WidgetBrowse* addWidgetBrowse(const Common::Hash& peerID, const Protos::Common::Entry& remoteEntry);

   private:
      WidgetSearch* addWidgetSearch(const QString& term, bool searchInOwnFiles = false);
      void removeAllWidgets();

      Ui::MainWindow* ui;

      QTabBar* mdiAreaTabBar;

      WidgetSettings* widgetSettings;
      WidgetChat* widgetChat;
      WidgetDownloads* widgetDownloads;
      WidgetUploads* widgetUploads;
      QList<WidgetBrowse*> widgetsBrowse;
      QList<WidgetSearch*> widgetsSearch;

      // This widget is shown on the tab of the downloads page. It is visible only after D-LAN has started and during the loading
      // of the cache (before the downloads are loaded).
      // This widget is owned by the tab bar of the 'QMdiArea'.
      BusyIndicator* downloadsBusyIndicator;

      QPoint dragPosition; // Used by custome styles.
      bool customStyleLoaded;
      Qt::WindowFlags initialWindowFlags;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      PeerListModel peerListModel;
      PeerListDelegate peerListDelegate;

      DirListModel sharedDirsModel;

      bool autoScroll;
      LogModel logModel;
      LogDelegate logDelegate;

      Taskbar taskbar;
   };
}

#endif
