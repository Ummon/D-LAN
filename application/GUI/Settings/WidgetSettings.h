/**
  * D-LAN - A decentralized LAN file sharing software.
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
  
#ifndef GUI_WIDGETSETTINGS_H
#define GUI_WIDGETSETTINGS_H

#include <QWidget>
#include <QDir>
#include <QStyledItemDelegate>
#include <QItemSelection>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Settings/DirListModel.h>

namespace Ui {
   class WidgetSettings;
}

namespace GUI
{
   class DirListDelegate : public QStyledItemDelegate
   {
      Q_OBJECT
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class WidgetSettings : public QWidget
   {
      Q_OBJECT
   public:
      explicit WidgetSettings(QSharedPointer<RCC::ICoreConnection> coreConnection, DirListModel& sharedDirsModel, QWidget *parent = 0);
      ~WidgetSettings();

      void coreConnected();
      void coreDisconnected();

   private slots:
      void newState(const Protos::GUI::State& state);
      void saveCoreSettings();
      void saveGUISettings();

      void addShared();
      void removeShared();

      void moveUpShared();
      void moveDownShared();

      void resetCoreAddress();

      void displayContextMenuDownload(const QPoint& point);
      void refreshButtonsAvailability(const QItemSelection& selected);
      void refreshButtonsAvailability();
      void openLocation();

      QStringList askForDirectories();

   protected:
      virtual void showEvent(QShowEvent* event);

   private:
      Ui::WidgetSettings* ui;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      DirListModel& sharedDirsModel;

      DirListDelegate dirListDelegate;

      bool initialState;
   };
}

#endif
