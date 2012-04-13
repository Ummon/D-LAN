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
  
#ifndef GUI_WIDGETSETTINGS_H
#define GUI_WIDGETSETTINGS_H

#include <QWidget>
#include <QDir>
#include <QStyledItemDelegate>
#include <QItemSelection>
#include <QVBoxLayout>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Settings/DirListModel.h>
#include <Settings/AskNewPasswordDialog.h>

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

      QString getCurrentLanguageFilename();

   public slots:
      void resetCoreAddress();
      void connectToCore();
      void disconnectFromTheCore();

   signals:
      void languageChanged(const QString& filename);

   private:
      void fillComboBoxLanguages();

      void connectAllAddressButtons();
      void disconnectAllAddressButtons();
      void updateNetworkInterfaces(const Protos::GUI::State& state);
      void updateAddresses(const Protos::Common::Interface& interface, QWidget* container);

   private slots:
      void newState(const Protos::GUI::State& state);
      void coreConnecting();
      void coreConnectingError();
      void coreConnected();
      void coreDisconnected();

      void saveCoreSettings();

      void cmbLanguageChanged(int cmbIndex);

      void changePassword();
      void resetPassword();

      void addShared();
      void removeShared();

      void moveUpShared();
      void moveDownShared();

      void displayContextMenuSharedDirs(const QPoint& point);
      void refreshButtonsAvailability(const QItemSelection& selected);
      void refreshButtonsAvailability();
      void openLocation();

      void buttonAddressToggled(bool checked);

   protected:
      bool eventFilter(QObject* obj, QEvent* event);
      void showEvent(QShowEvent* event);
      void changeEvent(QEvent* event);

   private:
      Ui::WidgetSettings* ui;

      // To avoid to send a state (via 'saveCoreSettings()') without getting at least one state (via 'newState(..)').
      bool getAtLeastOneState;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      DirListModel& sharedDirsModel;

      DirListDelegate dirListDelegate;

      bool corePasswordDefined;
   };
}

#endif
