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
  
#ifndef GUI_MAINWINDOW_H
#define GUI_MAINWINDOW_H

#include <QMainWindow>
#include <QLabel>
#include <QStyledItemDelegate>
#include <QIcon>
#include <QMdiSubWindow>
#include <QKeyEvent>

#include <Protos/gui_protocol.pb.h>
#include <Protos/common.pb.h>

#include <PeerListModel.h>
#include <LogModel.h>
#include <CoreConnection.h>

#include <WidgetSettings.h>
#include <WidgetChat.h>
#include <WidgetDownloads.h>
#include <WidgetUploads.h>
#include <WidgetBrowse.h>
#include <WidgetSearch.h>

namespace Ui {
   class MainWindow;
}

namespace GUI
{
   class PeerTableDelegate : public QStyledItemDelegate
   {
      Q_OBJECT
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class LogDelegate : public QStyledItemDelegate
   {
      Q_OBJECT
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class MainWindow : public QMainWindow
   {
      Q_OBJECT
   public:
      explicit MainWindow(QWidget* parent = 0);
      ~MainWindow();

   private slots:
      void coreConnected();
      void coreDisconnected();

      void displayContextMenuPeers(const QPoint& point);
      void browse();
      void search();

      void removeWidget(QWidget* widget);

      void newLogMessage();

   protected:
      void keyPressEvent(QKeyEvent* event);
      void closeEvent(QCloseEvent * event);

   private:
      void saveWindowsSettings();
      void restoreWindowsSettings();

      void removeMdiSubWindow(QMdiSubWindow* mdiSubWindow);

      void addWidgetSettings();

      void addWidgetChat();      
      void removeWidgetChat();

      void addWidgetDownloads();
      void removeWidgetDownloads();

      void addWidgetUploads();
      void removeWidgetUploads();

      void addWidgetBrowse(const Common::Hash& peerID);
      void addWidgetSearch(const QString& term);
      void removeAllWidgets();

      Ui::MainWindow* ui;

      WidgetSettings* widgetSettings;
      WidgetChat* widgetChat;
      WidgetDownloads* widgetDownloads;
      WidgetUploads* widgetUploads;
      QList<WidgetBrowse*> widgetsBrowse;
      QList<WidgetSearch*> widgetsSearch;

      CoreConnection coreConnection;

      PeerListModel peerListModel;
      PeerTableDelegate peerTableDelegate;

      LogModel logModel;
      LogDelegate logDelegate;
   };
}

#endif
