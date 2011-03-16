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
  
#include <Settings/WidgetSettings.h>
#include <ui_WidgetSettings.h>
using namespace GUI;

#include <QFileDialog>
#include <QMessageBox>
#include <QListView>
#include <QMenu>
#include <QDesktopServices>
#include <QUrl>

#include <Common/ProtoHelper.h>
#include <Common/Settings.h>

#include <Protos/gui_settings.pb.h>

#include <Settings/RemoteFileDialog.h>

void DirListDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);
   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

WidgetSettings::WidgetSettings(QSharedPointer<RCC::ICoreConnection> coreConnection, DirListModel& sharedDirsModel, QWidget* parent) :
   QWidget(parent), ui(new Ui::WidgetSettings), coreConnection(coreConnection), sharedDirsModel(sharedDirsModel), initialState(true)
{
   this->ui->setupUi(this);

   this->ui->tblShareDirs->setItemDelegate(&this->dirListDelegate);
   this->ui->tblShareDirs->setModel(&this->sharedDirsModel);
   this->ui->tblShareDirs->horizontalHeader()->setResizeMode(0, QHeaderView::Stretch);
   this->ui->tblShareDirs->horizontalHeader()->setResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblShareDirs->horizontalHeader()->setResizeMode(2, QHeaderView::ResizeToContents);
   this->ui->tblShareDirs->horizontalHeader()->setClickable(false);
   this->ui->tblShareDirs->horizontalHeader()->setVisible(true);

   this->ui->tblShareDirs->verticalHeader()->setResizeMode(QHeaderView::Fixed);
   this->ui->tblShareDirs->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 2);
   this->ui->tblShareDirs->verticalHeader()->setVisible(false);
   this->ui->tblShareDirs->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblShareDirs->setSelectionMode(QAbstractItemView::SingleSelection);
   this->ui->tblShareDirs->setShowGrid(false);
   this->ui->tblShareDirs->setAlternatingRowColors(true);

   this->ui->txtCoreAddress->setText(SETTINGS.get<QString>("core_address"));

   connect(this->coreConnection.data(), SIGNAL(newState(Protos::GUI::State)), this, SLOT(newState(Protos::GUI::State)));

   connect(this->ui->txtNick, SIGNAL(editingFinished()), this, SLOT(saveCoreSettings()));

   connect(this->ui->butAddShared, SIGNAL(clicked()), this, SLOT(addShared()));
   connect(this->ui->butRemoveShared, SIGNAL(clicked()), this, SLOT(removeShared()));

   connect(this->ui->butMoveUpShared, SIGNAL(clicked()), this, SLOT(moveUpShared()));
   connect(this->ui->butMoveDownShared, SIGNAL(clicked()), this, SLOT(moveDownShared()));

   connect(this->ui->butOpenFolder, SIGNAL(clicked()), this, SLOT(openLocation()));

   connect(this->ui->txtCoreAddress, SIGNAL(editingFinished()), this, SLOT(saveGUISettings()));
   connect(this->ui->txtPassword, SIGNAL(editingFinished()), this, SLOT(saveGUISettings()));
   connect(this->ui->butResetCoreAddress, SIGNAL(clicked()), this, SLOT(resetCoreAddress()));

   this->ui->tblShareDirs->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->tblShareDirs, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuDownload(const QPoint&)));   

   // When the selection change or a shared dir is moved/deleted/inserted we must set the availability of the action buttons.
   connect(this->ui->tblShareDirs->selectionModel(), SIGNAL(selectionChanged(const QItemSelection&, const QItemSelection&)), this, SLOT(refreshButtonsAvailability(const QItemSelection&)));
   connect(&this->sharedDirsModel, SIGNAL(layoutChanged()), this, SLOT(refreshButtonsAvailability()));
   connect(&this->sharedDirsModel, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(refreshButtonsAvailability()));
   connect(&this->sharedDirsModel, SIGNAL(rowsRemoved(const QModelIndex&, int, int)), this, SLOT(refreshButtonsAvailability()));
   this->refreshButtonsAvailability();
}

WidgetSettings::~WidgetSettings()
{
   delete this->ui;
}

void WidgetSettings::coreConnected()
{
   this->ui->txtPassword->clear();
   this->ui->tabWidget->setTabEnabled(0, true);
}

void WidgetSettings::coreDisconnected()
{
   this->initialState = true;
   this->ui->tabWidget->setTabEnabled(0, false);
}

void WidgetSettings::newState(const Protos::GUI::State& state)
{
   if (!this->ui->txtNick->hasFocus())
      this->ui->txtNick->setText(Common::ProtoHelper::getStr(state.myself(), &Protos::GUI::State_Peer::nick));

   QList<Common::SharedDir> sharedDirs;
   for (int i = 0; i < state.shared_directory_size(); i++)
      sharedDirs <<
         Common::SharedDir(
            state.shared_directory(i).id().hash().data(),
            Common::ProtoHelper::getStr(state.shared_directory(i), &Protos::GUI::State_SharedDir::path),
            state.shared_directory(i).size(),
            state.shared_directory(i).free_space()
         );
   this->sharedDirsModel.setDirs(sharedDirs);

   // If this is the first message state received and there is no incoming folder defined we ask the user to choose one.
   if (this->initialState)
   {
      this->initialState = false;
      if (this->sharedDirsModel.rowCount() == 0)
      {
         if (QMessageBox::question(
               this,
               "No shared folder",
               "You don't have any shared folder, would you like to choose one?",
               QMessageBox::Yes,
               QMessageBox::No
            ) == QMessageBox::Yes)
         {
            this->addShared();
         }
      }
   }
}

void WidgetSettings::saveCoreSettings()
{
   Protos::GUI::CoreSettings settings;
   Common::ProtoHelper::setStr(settings, &Protos::GUI::CoreSettings::set_nick, this->ui->txtNick->text());

   for (QListIterator<Common::SharedDir> i(this->sharedDirsModel.getDirs()); i.hasNext();)
      Common::ProtoHelper::addRepeatedStr(settings, &Protos::GUI::CoreSettings::add_shared_directory, i.next().path);

   this->coreConnection->setCoreSettings(settings);
}

void WidgetSettings::saveGUISettings()
{
   this->ui->txtCoreAddress->setText(this->ui->txtCoreAddress->text().trimmed());

   QString previousAddress = SETTINGS.get<QString>("core_address");
   SETTINGS.set("core_address", this->ui->txtCoreAddress->text());

   SETTINGS.set("password", Common::Hasher::hashWithSalt(this->ui->txtPassword->text()));

   SETTINGS.save();

   if (previousAddress != SETTINGS.get<QString>("core_address") || !this->coreConnection->isConnected())
   {
      this->coreConnection->connectToCore(SETTINGS.get<QString>("core_address"), SETTINGS.get<quint32>("core_port"), SETTINGS.get<Common::Hash>("password"));
   }
}

void WidgetSettings::addShared()
{
   QStringList dirs = askForDirectories();
   if (!dirs.isEmpty())
   {
      this->sharedDirsModel.addDirs(dirs);
      this->saveCoreSettings();
   }
}

void WidgetSettings::removeShared()
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

void WidgetSettings::moveUpShared()
{
   QModelIndex index = this->ui->tblShareDirs->selectionModel()->currentIndex();
   if (index.isValid())
   {
      this->sharedDirsModel.mvUpDir(index.row());
      this->saveCoreSettings();
   }
}

void WidgetSettings::moveDownShared()
{
   QModelIndex index = this->ui->tblShareDirs->selectionModel()->currentIndex();
   if (index.isValid())
   {
      this->sharedDirsModel.mvDownDir(index.row());
      this->saveCoreSettings();
   }
}

void WidgetSettings::resetCoreAddress()
{
   this->ui->txtCoreAddress->setText("localhost");
   this->saveGUISettings();
}

void WidgetSettings::displayContextMenuDownload(const QPoint& point)
{
   QPoint globalPosition = this->ui->tblShareDirs->mapToGlobal(point);
   globalPosition.setY(globalPosition.y() + this->ui->tblShareDirs->horizontalHeader()->height());
   if (this->coreConnection->isLocal())
   {
      QMenu menu;
      menu.addAction(QIcon(":/icons/ressources/delete.png"), "Remove the shared folder", this, SLOT(removeShared()));
      QAction* actionUp = menu.addAction(QIcon(":/icons/ressources/arrow_up.png"), "Move up", this, SLOT(moveUpShared()));
      QAction* actionDown = menu.addAction(QIcon(":/icons/ressources/arrow_down.png"), "Move down", this, SLOT(moveDownShared()));
      menu.addAction(QIcon(":/icons/ressources/explore_folder.png"), "Open location", this, SLOT(openLocation()));

      if (this->ui->tblShareDirs->currentIndex().row() == 0)
         actionUp->setDisabled(true);

      if (this->ui->tblShareDirs->currentIndex().row() >= this->sharedDirsModel.rowCount() - 1)
         actionDown->setDisabled(true);

      menu.exec(globalPosition);
   }
}

void WidgetSettings::refreshButtonsAvailability(const QItemSelection& selected)
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
      this->ui->butOpenFolder->setDisabled(false);
   }
}

void WidgetSettings::refreshButtonsAvailability()
{
   this->refreshButtonsAvailability(QItemSelection(this->ui->tblShareDirs->selectionModel()->selection()));
}

void WidgetSettings::openLocation()
{
   QModelIndexList selectedRows = this->ui->tblShareDirs->selectionModel()->selectedRows();
   foreach (QModelIndex index, selectedRows)
      QDesktopServices::openUrl(QUrl("file:///" + this->sharedDirsModel.getLocationPath(index), QUrl::TolerantMode));
}

/**
  * TODO : browse the remotes directories (Core) not the local ones.
  */
QStringList WidgetSettings::askForDirectories()
{
   if (this->coreConnection->isLocal())
   {
      QFileDialog fileDialog(this, "Choose a directory to share");
      fileDialog.setOption(QFileDialog::DontUseNativeDialog,true);
      fileDialog.setFileMode(QFileDialog::Directory);

      QListView* l = fileDialog.findChild<QListView*>("listView");
      if (l)
         l->setSelectionMode(QAbstractItemView::ExtendedSelection);

      if (fileDialog.exec())
      {
         return fileDialog.selectedFiles();
      }
      return QStringList();
   }
   else
   {
      RemoteFileDialog fileDialog(this);
      fileDialog.setWindowTitle("Remote folder");
      fileDialog.setText("Remote folder to share : ");
      if (fileDialog.exec())
      {
         return QStringList() << fileDialog.getFolder();
      }
      return QStringList();
   }
}

void WidgetSettings::showEvent(QShowEvent* event)
{
   if (this->ui->tabWidget->isTabEnabled(0))
      this->ui->tabWidget->setCurrentIndex(0);

   QWidget::showEvent(event);
}
