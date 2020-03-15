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
  
#include <Settings/SettingsWidget.h>
#include <ui_SettingsWidget.h>
using namespace GUI;

#include <QFileDialog>
#include <QTranslator>
#include <QMessageBox>
#include <QListView>
#include <QLabel>
#include <QMenu>
#include <QDesktopServices>
#include <QUrl>
#include <QStringBuilder>

#include <Common/Languages.h>
#include <Common/Constants.h>
#include <Common/ProtoHelper.h>
#include <Common/Settings.h>

#include <Protos/gui_settings.pb.h>

#include <Log.h>
#include <Utils.h>

void DirListDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItem newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);
   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

SettingsWidget::SettingsWidget(QSharedPointer<RCC::ICoreConnection> coreConnection, DirListModel& sharedDirsModel, QWidget* parent) :
   MdiWidget(parent),
   ui(new Ui::SettingsWidget),
   getAtLeastOneState(false),
   coreConnection(coreConnection),
   sharedDirsModel(sharedDirsModel),
   corePasswordDefined(false)
{
   this->ui->setupUi(this);

   this->ui->tblShareDirs->setItemDelegate(&this->dirListDelegate);
   this->ui->tblShareDirs->setModel(&this->sharedDirsModel);
   this->ui->tblShareDirs->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
   this->ui->tblShareDirs->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblShareDirs->horizontalHeader()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
   this->ui->tblShareDirs->horizontalHeader()->setSectionsClickable(false);
   this->ui->tblShareDirs->horizontalHeader()->setVisible(true);

   this->ui->tblShareDirs->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
   this->ui->tblShareDirs->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);
   this->ui->tblShareDirs->verticalHeader()->setVisible(false);
   this->ui->tblShareDirs->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblShareDirs->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblShareDirs->setShowGrid(false);
   this->ui->tblShareDirs->setAlternatingRowColors(true);

   this->ui->txtCoreAddress->setText(SETTINGS.get<QString>("core_address"));
   connect(this->ui->txtCoreAddress, SIGNAL(returnPressed()), this->ui->butConnect, SLOT(click()));
   connect(this->ui->txtPassword, SIGNAL(returnPressed()), this->ui->butConnect, SLOT(click()));

   connect(this->coreConnection.data(), SIGNAL(newState(Protos::GUI::State)), this, SLOT(newState(Protos::GUI::State)));
   connect(this->coreConnection.data(), SIGNAL(connecting()), this, SLOT(coreConnecting()));
   connect(this->coreConnection.data(), SIGNAL(connectingError(RCC::ICoreConnection::ConnectionErrorCode)), this, SLOT(coreConnectingError()));
   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected()));

   connect(this->ui->txtNick, SIGNAL(editingFinished()), this, SLOT(saveCoreSettings()));

   connect(this->ui->chkEnableIntegrityCheck, SIGNAL(clicked()), this, SLOT(saveCoreSettings()));
   connect(this->ui->butRefreshInterfaces, SIGNAL(clicked()), this, SLOT(refreshNetworkInterfaces()));

   this->connectAllAddressButtons();

   connect(this->ui->butAddShared, SIGNAL(clicked()), this, SLOT(addShared()));
   connect(this->ui->butRemoveShared, SIGNAL(clicked()), this, SLOT(removeShared()));

   connect(this->ui->butMoveUpShared, SIGNAL(clicked()), this, SLOT(moveUpShared()));
   connect(this->ui->butMoveDownShared, SIGNAL(clicked()), this, SLOT(moveDownShared()));

   connect(this->ui->butOpenFolder, SIGNAL(clicked()), this, SLOT(openLocation()));

   connect(this->ui->butResetCoreAddress, SIGNAL(clicked()), this, SLOT(resetCoreAddress()));
   connect(this->ui->butConnect, SIGNAL(clicked()), this, SLOT(connectToCore()));
   connect(this->ui->butDisconnect, SIGNAL(clicked()), this, SLOT(disconnectFromTheCore()));
   this->ui->tabAdvancedSettings->installEventFilter(this);

   this->ui->tblShareDirs->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->tblShareDirs, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuSharedDirs(const QPoint&)));
   connect(this->ui->tblShareDirs, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(openLocation()));

   // When the selection change or a shared dir is moved/deleted/inserted we must set the availability of the action buttons.
   connect(this->ui->tblShareDirs->selectionModel(), SIGNAL(selectionChanged(const QItemSelection&, const QItemSelection&)), this, SLOT(refreshButtonsAvailability(const QItemSelection&)));
   connect(&this->sharedDirsModel, SIGNAL(layoutChanged()), this, SLOT(refreshButtonsAvailability()));
   connect(&this->sharedDirsModel, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(refreshButtonsAvailability()));
   connect(&this->sharedDirsModel, SIGNAL(rowsRemoved(const QModelIndex&, int, int)), this, SLOT(refreshButtonsAvailability()));
   connect(&this->sharedDirsModel, SIGNAL(rowsMoved(const QModelIndex&, int, int, QModelIndex, int)), this, SLOT(refreshButtonsAvailability()));

   this->fillComboBoxLanguages();
   connect(this->ui->cmbLanguages, SIGNAL(currentIndexChanged(int)), this, SLOT(cmbLanguageChanged(int)));

   this->fillComboBoxStyles();
   connect(this->ui->cmbStyles, SIGNAL(currentIndexChanged(int)), this, SLOT(cmbStyleChanged(int)));
   connect(this->ui->butReloadStyle, SIGNAL(clicked()), this, SLOT(reloadCurrentStyle()));

   connect(this->ui->butChangePassword, SIGNAL(clicked()), this, SLOT(changePassword()));
   connect(this->ui->butResetPassword, SIGNAL(clicked()), this, SLOT(resetPassword()));

   this->refreshButtonsAvailability();
   this->coreDisconnected(); // To set the initial state.
}

SettingsWidget::~SettingsWidget()
{
   delete this->ui;
}

void SettingsWidget::resetCoreAddress()
{
   this->ui->txtCoreAddress->setText("localhost");
   this->connectToCore();
}

void SettingsWidget::connectToCore()
{
   const QString newHost = this->ui->txtCoreAddress->text().trimmed().toLower();

   if (newHost != SETTINGS.get<QString>("core_address") || !this->coreConnection->isConnected())
      this->coreConnection->connectToCore(newHost, SETTINGS.get<quint32>("core_port"), this->ui->txtPassword->text());
}

void SettingsWidget::disconnectFromTheCore()
{
   this->coreConnection->disconnectFromCore();
   SETTINGS.rm("password");
   SETTINGS.save();
}

/**
  * Read the available language files and fill the combo box.
  */
void SettingsWidget::fillComboBoxLanguages()
{
   QVariant dataEn;
   dataEn.setValue(Common::Language { "", QLocale("en") });
   this->ui->cmbLanguages->addItem("English", dataEn);

   QLocale current = QLocale::system();
   if (SETTINGS.isSet("language"))
      current = SETTINGS.get<QLocale>("language");

   this->coreConnection->setCoreLanguage(current);

   bool exactMatchFound = false;

   Common::Languages langs(QCoreApplication::applicationDirPath() + "/" + Common::Constants::LANGUAGE_DIRECTORY);
   for (QListIterator<Common::Language> i(langs.getAvailableLanguages(Common::Languages::ExeType::GUI)); i.hasNext();)
   {
      Common::Language lang = i.next();
      QVariant data;
      data.setValue(lang);
      this->ui->cmbLanguages->addItem(lang.locale.nativeLanguageName(), data);

      if (!exactMatchFound && lang.locale.language() == current.language())
      {
         exactMatchFound = lang.locale.country() == current.country();
         this->ui->cmbLanguages->setCurrentIndex(this->ui->cmbLanguages->count() - 1);
      }
   }
}

void SettingsWidget::fillComboBoxStyles()
{
   const QString& currentStyleFilename = SETTINGS.get<QString>("style");

   this->ui->cmbStyles->addItem(tr("Default"));

   const QDir styleDir(QCoreApplication::applicationDirPath() + "/" + Common::Constants::STYLE_DIRECTORY);
   for (QStringListIterator i(styleDir.entryList(QDir::Dirs | QDir::NoSymLinks | QDir::NoDotAndDotDot, QDir::Name)); i.hasNext();)
   {
      const QString& dirname = i.next();
      this->ui->cmbStyles->addItem(dirname, dirname);
      if (currentStyleFilename == dirname)
         this->ui->cmbStyles->setCurrentIndex(this->ui->cmbStyles->count() - 1);
   }
}

void SettingsWidget::connectAllAddressButtons()
{
   for (QListIterator<QRadioButton*> i(this->ui->scoInterfacesContent->findChildren<QRadioButton*>()); i.hasNext();)
      connect(i.next(), SIGNAL(toggled(bool)), this, SLOT(buttonAddressToggled(bool)));
}

void SettingsWidget::disconnectAllAddressButtons()
{
   for (QListIterator<QRadioButton*> i(this->ui->scoInterfacesContent->findChildren<QRadioButton*>()); i.hasNext();)
      i.next()->disconnect(this);
}

void SettingsWidget::updateNetworkInterfaces(const Protos::GUI::State& state)
{
   this->disconnectAllAddressButtons();

   QList<QLabel*> interfaceNotUpdated = this->ui->scoInterfacesContent->findChildren<QLabel*>("");

   for (int i = 0; i < state.interface_size(); i++)
   {
      const QString& interfaceName = Common::ProtoHelper::getStr(state.interface(i), &Protos::Common::Interface::name);

      for (QListIterator<QObject*> j(this->ui->scoInterfacesContent->children()); j.hasNext();)
      {
         QLabel* lblInterface = dynamic_cast<QLabel*>(j.next());
         if (lblInterface && lblInterface->property("id").toUInt() == state.interface(i).id())
         {
            interfaceNotUpdated.removeOne(lblInterface);
            lblInterface->setText(interfaceName + (state.interface(i).isup() ? "" : " <img src= \":/icons/ressources/error.png\" /> <em>" + tr("Interface not active") + "</em>"));
            this->updateAddresses(state.interface(i), static_cast<QWidget*>(j.next()));
            goto nextInterface;
         }
      }

      {
         // Interface not found -> add a new one.
         QLabel* label = new QLabel(interfaceName, this->ui->scoInterfacesContent);
         label->setProperty("id", state.interface(i).id());
         this->ui->layInterfaces->addWidget(label);
         QWidget* addressesContainer = new QWidget(this->ui->scoInterfacesContent);
         this->ui->layInterfaces->addWidget(addressesContainer);
         this->updateAddresses(state.interface(i), addressesContainer);
      }

      nextInterface:;
   }

   // Remove the non-existant interfaces.
   for (QListIterator<QObject*> i(this->ui->scoInterfacesContent->children()); i.hasNext();)
   {
      QLabel* current = dynamic_cast<QLabel*>(i.next());
      if (current && interfaceNotUpdated.contains(current))
      {
         this->ui->layInterfaces->removeWidget(current);
         QWidget* addressesContainer = dynamic_cast<QWidget*>(i.next());
         this->ui->layInterfaces->removeWidget(addressesContainer);
         delete current;
         delete addressesContainer;
      }
   }

   // Set the current address.
   if (state.has_listenany())
   {
      if (state.listenany() == Protos::Common::Interface::Address::IPv6)
         this->ui->radIPv6->setChecked(true);
      else
         this->ui->radIPv4->setChecked(true);
   }

   this->connectAllAddressButtons();
}

void SettingsWidget::updateAddresses(const Protos::Common::Interface& interfaceMess, QWidget* container)
{
   QVBoxLayout* layout = container->findChild<QVBoxLayout*>();
   if (!layout)
   {
      layout = new QVBoxLayout(container);
      QMargins margins = layout->contentsMargins();
      margins.setTop(3);
      layout->setContentsMargins(margins);
   }

   QList<QRadioButton*> addressesNotUpdated = container->findChildren<QRadioButton*>();

   for (int i = 0; i < interfaceMess.address_size(); i++)
   {
      const QString& addresseName = Common::ProtoHelper::getStr(interfaceMess.address(i), &Protos::Common::Interface::Address::address);

      for (QListIterator<QRadioButton*> j(container->findChildren<QRadioButton*>()); j.hasNext();)
      {
         QRadioButton* addressButton = j.next();
         if (addressButton->text() == addresseName)
         {
            addressesNotUpdated.removeOne(addressButton);
            if (interfaceMess.address(i).listened())
               addressButton->setChecked(true);
            goto nextAddress;
         }
      }

      {
         // Address not found -> add a new one.
         QRadioButton* newAddressButton = new QRadioButton(addresseName, container);
         this->ui->grpAddressesToListenTo->addButton(newAddressButton);
         if (interfaceMess.address(i).listened())
            newAddressButton->setChecked(true);
         layout->addWidget(newAddressButton);
      }

      nextAddress:;
   }

   // Remove the non-existant addresses.
   for (QListIterator<QRadioButton*> i(container->findChildren<QRadioButton*>()); i.hasNext();)
   {
      QRadioButton* current = i.next();
      if (addressesNotUpdated.contains(current))
      {
         layout->removeWidget(current);
         this->ui->grpAddressesToListenTo->removeButton(current);
         delete current;
      }
   }
}

void SettingsWidget::newState(const Protos::GUI::State& state)
{
   if (!this->ui->txtNick->hasFocus())
      this->ui->txtNick->setText(Common::ProtoHelper::getStr(state.peer(0), &Protos::GUI::State_Peer::nick));

   if (!this->ui->chkEnableIntegrityCheck->hasFocus())
      this->ui->chkEnableIntegrityCheck->setChecked(state.integrity_check_enabled());

   if (this->corePasswordDefined = state.password_defined())
   {
      this->ui->txtPassword->setPlaceholderText("");
      this->ui->butResetPassword->setEnabled(true);
      this->ui->butChangePassword->setText(tr("Change the password"));
   }
   else
   {
      this->ui->txtPassword->setPlaceholderText("No password defined");
      this->ui->butResetPassword->setEnabled(false);
      this->ui->butChangePassword->setText(tr("Define a password"));
   }

   QList<Common::SharedDir> sharedDirs;
   for (int i = 0; i < state.shared_directory_size(); i++)
      sharedDirs <<
         Common::SharedDir {
            state.shared_directory(i).id().hash(),
            Common::ProtoHelper::getStr(state.shared_directory(i), &Protos::GUI::State_SharedDir::path),
            (qint64)state.shared_directory(i).size(),
            (qint64)state.shared_directory(i).free_space()
         };
   this->sharedDirsModel.setDirs(sharedDirs);

   this->updateNetworkInterfaces(state);

   this->getAtLeastOneState = true;


   // If this is the first message state received and there is no incoming folder defined we ask the user to choose one.
   // Commented cuz the user can know choose a folder right before downloading a file.
   /*if (this->initialState)
   {
      this->initialState = false;
      if (this->sharedDirsModel.rowCount() == 0)
      {
         if (QMessageBox::question(
               this,
               "No directory folder",
               "You don't have any shared directory, would you like to choose one?",
               QMessageBox::Yes,
               QMessageBox::No
            ) == QMessageBox::Yes)
         {
            this->addShared();
         }
      }
   }*/
}

void SettingsWidget::coreConnecting()
{
   this->ui->butConnect->setDisabled(true);
   this->ui->butDisconnect->setDisabled(true);
   this->ui->butResetCoreAddress->setDisabled(true);
   this->ui->butConnect->setText(tr("Connecting . . ."));
}

void SettingsWidget::coreConnectingError()
{
   this->ui->butConnect->setDisabled(false);
   this->ui->butDisconnect->setDisabled(!this->coreConnection->isConnected());
   this->ui->butResetCoreAddress->setDisabled(false);
   this->ui->butConnect->setText(tr("Connect"));
}

void SettingsWidget::coreConnected()
{
   SETTINGS.set("core_address", this->coreConnection->getConnectionInfo().address);
   SETTINGS.set("core_port", static_cast<quint32>(this->coreConnection->getConnectionInfo().port));
   SETTINGS.set("password", this->coreConnection->getConnectionInfo().password);
   SETTINGS.save();

   this->ui->txtPassword->clear();
   this->ui->tabWidget->setTabEnabled(0, true);
   this->ui->tabWidget->setTabEnabled(1, true);
   this->ui->chkEnableIntegrityCheck->setEnabled(true);

   this->ui->butConnect->setDisabled(false);
   this->ui->butConnect->setText(tr("Connect"));
   this->ui->butDisconnect->setDisabled(false);
   this->ui->butResetCoreAddress->setDisabled(false);

   this->ui->butChangePassword->setDisabled(false);

   this->ui->butOpenFolder->setDisabled(!this->coreConnection->isLocal());
}

void SettingsWidget::coreDisconnected()
{
   this->getAtLeastOneState = false;

   this->ui->tabWidget->setTabEnabled(0, false);
   this->ui->tabWidget->setTabEnabled(1, false);
   this->ui->chkEnableIntegrityCheck->setEnabled(false);

   this->ui->butConnect->setDisabled(false);
   this->ui->butConnect->setText(tr("Connect"));
   this->ui->butDisconnect->setDisabled(true);

   this->ui->butChangePassword->setDisabled(true);
   this->ui->butResetPassword->setDisabled(true);
}

void SettingsWidget::refreshNetworkInterfaces()
{
   this->coreConnection->refreshNetworkInterfaces();
}

/**
  * Send the settings to the core. A connection to a core must be established.
  */
void SettingsWidget::saveCoreSettings()
{
   if (!this->getAtLeastOneState)
      return;

   Protos::GUI::CoreSettings settings;
   Common::ProtoHelper::setStr(settings, &Protos::GUI::CoreSettings::set_nick, this->ui->txtNick->text());
   settings.set_enable_integrity_check(this->ui->chkEnableIntegrityCheck->isChecked());

   for (QListIterator<Common::SharedDir> i(this->sharedDirsModel.getDirs()); i.hasNext();)
      Common::ProtoHelper::addRepeatedStr(*settings.mutable_shared_directories(), &Protos::GUI::CoreSettings::SharedDirectories::add_dir, i.next().path);

   if (this->ui->radIPv6->isChecked())
      settings.set_listen_any(Protos::Common::Interface::Address::IPv6);
   else if (this->ui->radIPv4->isChecked())
      settings.set_listen_any(Protos::Common::Interface::Address::IPv4);
   else
   {
      for (QListIterator<QRadioButton*> i(this->ui->grpInterfaces->findChildren<QRadioButton*>()); i.hasNext();)
      {
         QRadioButton* button = i.next();
         if (button->isChecked())
         {
            Common::ProtoHelper::setStr(settings, &Protos::GUI::CoreSettings::set_listen_address, button->text());
            break;
         }
      }
   }

   this->coreConnection->setCoreSettings(settings);
}

void SettingsWidget::cmbLanguageChanged(int cmbIndex)
{
   const Common::Language& lang = this->ui->cmbLanguages->itemData(cmbIndex).value<Common::Language>();
   emit languageChanged(lang.filename);
   this->coreConnection->setCoreLanguage(lang.locale);
   SETTINGS.set("language", this->ui->cmbLanguages->itemData(this->ui->cmbLanguages->currentIndex()).value<Common::Language>().locale);
   SETTINGS.save();
}

void SettingsWidget::cmbStyleChanged(int cmbIndex)
{
   const QString& dirname = this->ui->cmbStyles->itemData(cmbIndex).toString();
   emit styleChanged(dirname.isEmpty() ? QString() : QCoreApplication::applicationDirPath() % "/" % Common::Constants::STYLE_DIRECTORY % "/" % dirname % "/" % Common::Constants::STYLE_FILE_NAME);
   SETTINGS.set("style", dirname);
   SETTINGS.save();
}

void SettingsWidget::reloadCurrentStyle()
{
   const QString& dirname = this->ui->cmbStyles->itemData(this->ui->cmbStyles->currentIndex()).toString();
   emit styleChanged(dirname.isEmpty() ? QString() : QCoreApplication::applicationDirPath() % "/" % Common::Constants::STYLE_DIRECTORY % "/" % dirname % "/" % Common::Constants::STYLE_FILE_NAME);
}

void SettingsWidget::changePassword()
{
   AskNewPasswordDialog dia(this->coreConnection, this->corePasswordDefined, this);
   dia.exec();
}

void SettingsWidget::resetPassword()
{
   this->coreConnection->resetCorePassword();
   if (!this->coreConnection->isLocal())
      this->coreConnection->disconnectFromCore();
}

void SettingsWidget::addShared()
{
   QStringList dirs = Utils::askForDirectories(this->coreConnection);
   if (!dirs.isEmpty())
   {
      this->sharedDirsModel.addDirs(dirs);
      this->saveCoreSettings();
   }
}

void SettingsWidget::removeShared()
{
   QModelIndex index = this->ui->tblShareDirs->selectionModel()->currentIndex();
   if (index.isValid())
   {
      QMessageBox msgBox(this);
      msgBox.setWindowTitle("Remove selected shared directory");
      msgBox.setText("Are you sure to remove the selected shared directory? All computed hashes will be lost.");
      msgBox.setIcon(QMessageBox::Question);
      msgBox.setStandardButtons(QMessageBox::Ok | QMessageBox::Cancel);
      msgBox.setDefaultButton(QMessageBox::Ok);
      if (msgBox.exec() == QMessageBox::Ok)
      {
         this->sharedDirsModel.rmDir(index.row());
         this->saveCoreSettings();
      }
   }
}

void SettingsWidget::moveUpShared()
{
   QModelIndex index = this->ui->tblShareDirs->selectionModel()->currentIndex();
   if (index.isValid())
   {
      this->sharedDirsModel.mvUpDir(index.row());
      this->saveCoreSettings();
   }
}

void SettingsWidget::moveDownShared()
{
   QModelIndex index = this->ui->tblShareDirs->selectionModel()->currentIndex();
   if (index.isValid())
   {
      this->sharedDirsModel.mvDownDir(index.row());
      this->saveCoreSettings();
   }
}

void SettingsWidget::displayContextMenuSharedDirs(const QPoint& point)
{
   QPoint globalPosition = this->ui->tblShareDirs->mapToGlobal(point);
   globalPosition.setY(globalPosition.y() + this->ui->tblShareDirs->horizontalHeader()->height());

   QMenu menu;
   QAction* actionDelete = menu.addAction(QIcon(":/icons/ressources/delete.png"), tr("Remove the shared directory"), this, SLOT(removeShared()));
   QAction* actionUp = menu.addAction(QIcon(":/icons/ressources/arrow_up.png"), tr("Move up"), this, SLOT(moveUpShared()));
   QAction* actionDown = menu.addAction(QIcon(":/icons/ressources/arrow_down.png"), tr("Move down"), this, SLOT(moveDownShared()));

   if (this->coreConnection->isLocal() && this->sharedDirsModel.rowCount() > 0)
      menu.addAction(QIcon(":/icons/ressources/explore_folder.png"), tr("Open location"), this, SLOT(openLocation()));

   if (this->sharedDirsModel.rowCount() == 0)
      actionDelete->setDisabled(true);

   if (this->ui->tblShareDirs->currentIndex().row() == 0 || this->sharedDirsModel.rowCount() == 0)
      actionUp->setDisabled(true);

   if (this->ui->tblShareDirs->currentIndex().row() >= this->sharedDirsModel.rowCount() - 1  || this->sharedDirsModel.rowCount() == 0)
      actionDown->setDisabled(true);

   menu.exec(globalPosition);
}

void SettingsWidget::refreshButtonsAvailability(const QItemSelection& selected)
{
   if (selected.indexes().isEmpty() || !selected.indexes().first().isValid())
   {
      this->ui->butMoveUpShared->setDisabled(true);
      this->ui->butMoveDownShared->setDisabled(true);
      this->ui->butRemoveShared->setDisabled(true);
      this->ui->butOpenFolder->setDisabled(true);
   }
   else
   {
      this->ui->butMoveUpShared->setDisabled(selected.indexes().first().row() == 0);
      this->ui->butMoveDownShared->setDisabled(selected.indexes().first().row() == this->sharedDirsModel.rowCount() - 1);
      this->ui->butRemoveShared->setDisabled(false);
      this->ui->butOpenFolder->setDisabled(!this->coreConnection->isLocal());
   }
}

void SettingsWidget::refreshButtonsAvailability()
{
   this->refreshButtonsAvailability(QItemSelection(this->ui->tblShareDirs->selectionModel()->selection()));
}

void SettingsWidget::openLocation()
{
   QModelIndexList selectedRows = this->ui->tblShareDirs->selectionModel()->selectedRows();
   foreach (QModelIndex index, selectedRows)
      QDesktopServices::openUrl(QUrl("file:///" + this->sharedDirsModel.getLocationPath(index), QUrl::TolerantMode));
}

void SettingsWidget::buttonAddressToggled(bool checked)
{
   if (checked)
      this->saveCoreSettings();
}

bool SettingsWidget::eventFilter(QObject* obj, QEvent* event)
{
   if (obj == this->ui->tabAdvancedSettings && event->type() == QEvent::Show)
   {
      this->ui->txtPassword->clear();
      this->ui->txtCoreAddress->setText(SETTINGS.get<QString>("core_address"));
   }

   return QObject::eventFilter(obj, event);
}

void SettingsWidget::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);

   MdiWidget::changeEvent(event);
}

void SettingsWidget::onActivate()
{
   if (this->ui->tabWidget->isTabEnabled(0))
      this->ui->tabWidget->setCurrentIndex(0);
}

