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

#include <QWidget>
#include <QDir>
#include <QStyledItemDelegate>
#include <QItemSelection>
#include <QVBoxLayout>

#include <Common/RemoteCoreController/ICoreConnection.h>

#include <Settings/SharedEntryListModel.h>
#include <Settings/AskNewPasswordDialog.h>
#include <MDI/MdiWidget.h>

namespace Ui {
   class SettingsWidget;
}

namespace GUI
{
   class DirListDelegate : public QStyledItemDelegate
   {
      Q_OBJECT
   public:
      void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const;
   };

   class SettingsWidget : public MdiWidget
   {
      Q_OBJECT

   public:
      explicit SettingsWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, SharedEntryListModel& sharedEntryListModel, QWidget* parent = nullptr);
      ~SettingsWidget();

   public slots:
      void resetCoreAddress();
      void connectToCore();
      void disconnectFromTheCore();

   signals:
      void languageChanged(const QString& filename);
      void styleChanged(const QString& filename);

   private:
      void fillComboBoxLanguages();
      void fillComboBoxStyles();

      void connectAllAddressButtons();
      void disconnectAllAddressButtons();
      void updateNetworkInterfaces(const Protos::GUI::State& state);
      void updateAddresses(const Protos::Common::Interface& interfaceMess, QWidget* container);

   private slots:
      void newState(const Protos::GUI::State& state);
      void coreConnecting();
      void coreConnectingError();
      void coreConnected();
      void coreDisconnected();

      void refreshNetworkInterfaces();

      void saveCoreSettings();

      void cmbLanguageChanged(int cmbIndex);

      void cmbStyleChanged(int cmbIndex);
      void reloadCurrentStyle();

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
      void changeEvent(QEvent* event);

   private:
      void onActivate();

      Ui::SettingsWidget* ui;

      // To avoid to send a state (via 'saveCoreSettings()') without getting at least one state (via 'newState(..)').
      bool getAtLeastOneState;

      QSharedPointer<RCC::ICoreConnection> coreConnection;

      SharedEntryListModel& sharedEntryListModel;

      DirListDelegate dirListDelegate;

      bool corePasswordDefined;
   };
}
