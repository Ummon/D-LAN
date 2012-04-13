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
  
#include <Downloads/WidgetDownloads.h>
#include <ui_WidgetDownloads.h>
using namespace GUI;

#include <limits>

#include <QMenu>
#include <QMessageBox>
#include <QDesktopServices>
#include <QUrl>

#include <Common/Global.h>
#include <Common/Settings.h>

void DownloadsDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   if (!index.isValid())
      return;

   if (index.column() == 2)
   {
      Progress progress = index.data().value<Progress>();

      QStyleOptionProgressBarV2 progressBarOption;
      progressBarOption.QStyleOption::operator=(option);

      progressBarOption.minimum = 0;
      progressBarOption.maximum = 10000;
      progressBarOption.textAlignment = Qt::AlignHCenter;
      progressBarOption.progress = progress.progress;

      switch (progress.status)
      {
      case Protos::GUI::State::Download::QUEUED:
         progressBarOption.text = "Queued";
         break;
      case Protos::GUI::State::Download::GETTING_THE_HASHES:
         progressBarOption.text = "Getting the hashes..";
         break;
      case Protos::GUI::State::Download::DOWNLOADING:
         progressBarOption.text = QString("%1%").arg(static_cast<double>(progress.progress) / 100);
         break;
      case Protos::GUI::State::Download::COMPLETE:
         progressBarOption.text = "Complete";
         break;
      case Protos::GUI::State::Download::PAUSED:
         progressBarOption.text = "Paused";
         break;
      default:
         progressBarOption.text = "Waiting..";
         break;
      }

      progressBarOption.textVisible = true;

      QApplication::style()->drawControl(QStyle::CE_ProgressBar, &progressBarOption, painter);
   }
   else
   {
      // Remove the focus box, not very useful.
      QStyleOptionViewItemV4 newOption(option);
      newOption.state = option.state & (~QStyle::State_HasFocus);

      if (index.column() == 3 && !static_cast<const DownloadsModel*>(index.model())->isSourceAlive(index))
         newOption.font.setStrikeOut(true);

      QStyledItemDelegate::paint(painter, newOption, index);
   }
}

QSize DownloadsDelegate::sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QSize size = QStyledItemDelegate::sizeHint(option, index);

   switch (index.column())
   {
   case 2:
      size.setWidth(120);
      break;
   case 4:
      if (index.data().toString().isEmpty())
         size.setWidth(0);
      break;
   }

   return size;
}

/////

WidgetDownloads::WidgetDownloads(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, QWidget *parent) :
   QWidget(parent),
   ui(new Ui::WidgetDownloads),
   coreConnection(coreConnection),
   downloadsFlatModel(coreConnection, peerListModel, sharedDirsModel, checkBoxModel),
   downloadsTreeModel(coreConnection, peerListModel, sharedDirsModel, checkBoxModel),
   currentDownloadsModel(0),
   treeViewState(new SimpleTree<quint32>())
{
   this->ui->setupUi(this);

   this->switchView(static_cast<Protos::GUI::Settings::DownloadView>(SETTINGS.get<quint32>("download_view")));

   this->ui->tblDownloads->setItemDelegate(&this->downloadsDelegate);

   this->ui->tblDownloads->setDragEnabled(true);
   this->ui->tblDownloads->setDragDropMode(QAbstractItemView::InternalMove);
   this->ui->tblDownloads->setDropIndicatorShown(true);

   this->ui->tblDownloads->setVerticalScrollMode(QAbstractItemView::ScrollPerPixel);
   this->ui->tblDownloads->header()->setStretchLastSection(false);
   this->ui->tblDownloads->header()->setVisible(false);
   this->ui->tblDownloads->header()->setResizeMode(0, QHeaderView::Stretch);
   this->ui->tblDownloads->header()->setResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->tblDownloads->header()->setResizeMode(2, QHeaderView::ResizeToContents);
   this->ui->tblDownloads->header()->setResizeMode(3, QHeaderView::ResizeToContents);
   this->ui->tblDownloads->header()->setResizeMode(4, QHeaderView::ResizeToContents);
   this->ui->tblDownloads->header()->setMinimumSectionSize(0);

   this->ui->tblDownloads->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblDownloads->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->tblDownloads->setAlternatingRowColors(true);

   this->ui->tblDownloads->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->tblDownloads, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuDownloads(const QPoint&)));
   connect(this->ui->tblDownloads, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(downloadDoubleClicked(const QModelIndex&)));

   connect(&this->downloadsFlatModel, SIGNAL(globalProgressChanged()), this, SLOT(updateGlobalProgressBar()));

   connect(this->ui->butRemoveComplete, SIGNAL(clicked()), this, SLOT(removeCompletedFiles()));
   connect(this->ui->butRemoveSelected, SIGNAL(clicked()), this, SLOT(removeSelectedEntries()));
   connect(this->ui->butPause, SIGNAL(clicked()), this, SLOT(pauseSelectedEntries()));
   connect(this->ui->butSwitchView, SIGNAL(clicked()), this, SLOT(switchView()));

   this->filterStatusList = new CheckBoxList(this);
   this->filterStatusList->setModel(&this->checkBoxModel);
   this->updateCheckBoxElements();
   this->ui->layTools->insertWidget(1, this->filterStatusList);

   connect(&this->checkBoxModel, SIGNAL(dataChanged(QModelIndex,QModelIndex)), this, SLOT(filterChanged()));
}

WidgetDownloads::~WidgetDownloads()
{
   delete this->ui;
}

void WidgetDownloads::keyPressEvent(QKeyEvent* event)
{
   if (event->key() == Qt::Key_Delete || event->key() == Qt::Key_Backspace)
      this->removeSelectedEntries();
   else
      QWidget::keyPressEvent(event);
}

void WidgetDownloads::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
   {
      this->ui->retranslateUi(this);
      this->updateCheckBoxElements();
   }
   else
      QWidget::changeEvent(event);
}

void WidgetDownloads::displayContextMenuDownloads(const QPoint& point)
{   
   QModelIndexList selectedRows = this->ui->tblDownloads->selectionModel()->selectedRows();

   QMenu menu;

   // If the connection isn't remote and there is at least one complete or downloading file we show a menu action to open the file location.
   if (this->coreConnection->isLocal())
      for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
         if (this->currentDownloadsModel->isFileLocationKnown(i.next()))
         {
            menu.addAction(QIcon(":/icons/ressources/explore_folder.png"), tr("Open location"), this, SLOT(openLocationSelectedEntries()));
            break;
         }

   menu.addAction(QIcon(":/icons/ressources/arrow_up.png"), tr("Move to top"), this, SLOT(moveSelectedEntriesToTop()));

   menu.addAction(QIcon(":/icons/ressources/remove_complete_files.png"), tr("Remove completed files"), this, SLOT(removeCompletedFiles()));
   menu.addAction(QIcon(":/icons/ressources/delete.png"), tr("Remove selected entries"), this, SLOT(removeSelectedEntries()));

   QPair<QList<quint64>, bool> IDs = this->getDownloadIDsToPause();
   if (!IDs.first.isEmpty())
      menu.addAction(QIcon(":/icons/ressources/pause.png"), IDs.second ? tr("Pause selected entries") : tr("Unpause selected entries"), this, SLOT(pauseSelectedEntries()));

   menu.exec(this->ui->tblDownloads->mapToGlobal(point));
}

void WidgetDownloads::downloadDoubleClicked(const QModelIndex& index)
{
   if (this->currentDownloadsModel->getType(index) == Protos::Common::Entry::FILE && this->currentDownloadsModel->isFileLocationKnown(index))
      QDesktopServices::openUrl(QUrl("file:///" + this->currentDownloadsModel->getPath(index)));
}

void WidgetDownloads::openLocationSelectedEntries()
{
   QModelIndexList selectedRows = this->ui->tblDownloads->selectionModel()->selectedRows();

   QSet<QString> locations;
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      const QModelIndex& index = i.next();
      if (this->currentDownloadsModel->isFileLocationKnown(index))
         locations.insert("file:///" + this->currentDownloadsModel->getPath(index, false));
   }

   for (QSetIterator<QString> i(locations); i.hasNext();)
      QDesktopServices::openUrl(QUrl(i.next(), QUrl::TolerantMode));
}

void WidgetDownloads::moveSelectedEntriesToTop()
{
   QSet<quint64> downloadIDs;

   QModelIndexList selectedRows = this->ui->tblDownloads->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      const QModelIndex& index = i.next();
      downloadIDs += this->currentDownloadsModel->getDownloadIDs(index).toSet();
   }

   // Search a download which is not selected
   for (int r = 0; r < this->downloadsFlatModel.rowCount(); r++)
   {
      const quint64 id = this->downloadsFlatModel.getDownloadIDs(this->downloadsFlatModel.index(r, 0)).first();
      if (!downloadIDs.contains(id))
      {
         this->coreConnection->moveDownloads(QList<quint64>() << id, downloadIDs.toList());
         break;
      }
   }

   this->ui->tblDownloads->selectionModel()->clear();
}

void WidgetDownloads::switchView()
{
   if (this->currentDownloadsModel == &this->downloadsFlatModel)
      this->switchView(Protos::GUI::Settings::TREE_VIEW);
   else
      this->switchView(Protos::GUI::Settings::LIST_VIEW);
}

void WidgetDownloads::removeCompletedFiles()
{
   this->coreConnection->cancelDownloads(QList<quint64>(), true);
}

void WidgetDownloads::removeSelectedEntries()
{
   QSet<quint64> downloadIDs;

   QModelIndexList selectedRows = this->ui->tblDownloads->selectionModel()->selectedRows();
   bool allComplete = true;
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      const QModelIndex& index = i.next();
      downloadIDs += this->currentDownloadsModel->getDownloadIDs(index).toSet();
      if (!this->currentDownloadsModel->isFileComplete(index))
         allComplete = false;
   }

   if (!downloadIDs.isEmpty())
   {
      if (!allComplete)
      {
         QMessageBox msgBox(this);
         msgBox.setWindowIcon(QIcon(":/icons/ressources/delete.png"));
         msgBox.setWindowTitle("Remove selected downloads");
         msgBox.setText("Are you sure to remove the selected downloads? There is one or more unfinished download.");
         msgBox.setIcon(QMessageBox::Question);
         msgBox.setStandardButtons(QMessageBox::Ok | QMessageBox::Cancel);
         msgBox.setDefaultButton(QMessageBox::Ok);
         if (msgBox.exec() == QMessageBox::Ok)
            this->coreConnection->cancelDownloads(downloadIDs.toList());
      }
      else
         this->coreConnection->cancelDownloads(downloadIDs.toList());
   }
}

void WidgetDownloads::pauseSelectedEntries()
{
   QPair<QList<quint64>, bool> IDs = this->getDownloadIDsToPause();

   if (!IDs.first.isEmpty())
      this->coreConnection->pauseDownloads(IDs.first, IDs.second);
}

void WidgetDownloads::filterChanged()
{
   this->coreConnection->refresh();
}

void WidgetDownloads::updateGlobalProgressBar()
{
   const quint64 bytesInQueue = this->downloadsFlatModel.getTotalBytesInQueue();
   const quint64 bytesDownloaded = this->downloadsFlatModel.getTotalBytesDownloadedInQueue();
   const quint64 eta = this->downloadsFlatModel.getEta();

   this->ui->prgGlobalProgress->setValue(bytesInQueue == 0 ? 0 : bytesDownloaded * 10000LL / bytesInQueue);

   if (bytesInQueue == 0)
      this->ui->prgGlobalProgress->setFormat("");
   else
      this->ui->prgGlobalProgress->setFormat(
         QString("%1 / %2%3")
            .arg(Common::Global::formatByteSize(bytesDownloaded))
            .arg(Common::Global::formatByteSize(bytesInQueue))
            .arg(eta == 0 || eta > 604800 ? "" : " (" + Common::Global::formatTime(eta) + ")")
            );
}

void WidgetDownloads::switchView(Protos::GUI::Settings::DownloadView view)
{
   if (view == Protos::GUI::Settings::TREE_VIEW)
   {
      this->ui->butSwitchView->setIcon(QIcon(":/icons/ressources/list_view.png"));
      this->ui->butSwitchView->setToolTip(tr("Switch to file list view"));
      this->ui->tblDownloads->setIndentation(20);

      if (this->currentDownloadsModel != &this->downloadsTreeModel)
      {
         this->currentDownloadsModel = &this->downloadsTreeModel;
         this->ui->tblDownloads->setModel(this->currentDownloadsModel);
         this->restoreTreeViewState();
      }
   }
   else if (view == Protos::GUI::Settings::LIST_VIEW)
   {
      this->ui->butSwitchView->setIcon(QIcon(":/icons/ressources/tree_view.png"));
      this->ui->butSwitchView->setToolTip(tr("Switch to tree view"));
      this->ui->tblDownloads->setIndentation(0);

      if (this->currentDownloadsModel != &this->downloadsFlatModel)
      {
         this->saveTreeViewState();
         this->currentDownloadsModel = &this->downloadsFlatModel;
         this->ui->tblDownloads->setModel(this->currentDownloadsModel);
      }
   }

   SETTINGS.set("download_view", static_cast<quint32>(view));
   SETTINGS.save();
}

void WidgetDownloads::updateCheckBoxElements()
{
   this->checkBoxModel.clear(tr("<All>"));
   this->checkBoxModel.addElement(tr("Complete"), true, STATUS_COMPLETE);
   this->checkBoxModel.addElement(tr("Downloading"), true, STATUS_DOWNLOADING);
   this->checkBoxModel.addElement(tr("Queued"), true, STATUS_QUEUED);
   this->checkBoxModel.addElement(tr("Inactive"), true, STATUS_INACTIVE);
}

QPair<QList<quint64>, bool> WidgetDownloads::getDownloadIDsToPause() const
{
   QSet<quint64> downloadIDs; // We use a set to avoid equal IDs.

   QModelIndexList selectedRows = this->ui->tblDownloads->selectionModel()->selectedRows();
   bool allPaused = true;
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      const QModelIndex& index = i.next();
      foreach (quint64 ID, this->currentDownloadsModel->getDownloadIDs(index))
         if (!this->currentDownloadsModel->isFileComplete(index))
         {
            downloadIDs.insert(ID);
            if (!this->currentDownloadsModel->isDownloadPaused(index))
               allPaused = false;
         }
   }

   return qMakePair(downloadIDs.toList(), !allPaused);
}

/**
  * Save the opened directories in 'treeViewState'. Save only hash(item_name) to save space and to facilitate a future serialization.
  */
void WidgetDownloads::saveTreeViewState()
{
   if (this->currentDownloadsModel != &this->downloadsTreeModel)
      return;

   this->treeViewState->deleteAllChildren();
   this->saveTreeViewState(QModelIndex(), this->treeViewState);
}

void WidgetDownloads::saveTreeViewState(const QModelIndex& index, SimpleTree<quint32>* tree)
{
   for (int row = 0; row < this->downloadsTreeModel.rowCount(index); row++)
   {
      const QModelIndex& childIndex = this->downloadsTreeModel.index(row, 0, index);
      if (this->ui->tblDownloads->isExpanded(childIndex))
         this->saveTreeViewState(childIndex, tree->insertChild(Common::Global::hashStringToInt(this->downloadsTreeModel.data(childIndex).toString())));
   }
}

/**
  * Restore the opened directories from 'treeViewState'.
  */
void WidgetDownloads::restoreTreeViewState()
{
   if (this->currentDownloadsModel != &this->downloadsTreeModel)
      return;

   this->restoreTreeViewState(QModelIndex(), this->treeViewState);
}

void WidgetDownloads::restoreTreeViewState(const QModelIndex& index, SimpleTree<quint32>* tree)
{
   // O(n^2)
   for (int row = 0; row < this->downloadsTreeModel.rowCount(index); row++)
   {
      const QModelIndex& childIndex = this->downloadsTreeModel.index(row, 0, index);
      const quint32 currentHash = Common::Global::hashStringToInt(this->downloadsTreeModel.data(childIndex).toString());

      for (int i = 0; i < tree->getNbChildren(); i++)
         if (tree->getChild(i)->getItem() == currentHash)
         {
            this->ui->tblDownloads->expand(childIndex);
            this->restoreTreeViewState(childIndex, tree->getChild(i));
            break;
         }
   }
}
