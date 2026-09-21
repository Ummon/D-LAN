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

#include <RemoteBrowseDialog/RemoteBrowseDialog.h>
#include <qmenu.h>
#include <ui_RemoteBrowseDialog.h>
using namespace GUI;

#include <Common/Global.h>
#include <Common/Settings.h>

#include <Utils.h>

void RemoteBrowseDialogDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItem newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);
   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

RemoteBrowseDialog::RemoteBrowseDialog(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget *parent) :
   QDialog(parent),
   ui(new Ui::RemoteBrowseDialog),
   model(coreConnection),
   modelQuickAccess(coreConnection)
{
   this->ui->setupUi(this);

   this->ui->treeView->setModel(&this->model);
   this->ui->treeView->setItemDelegate(&this->delegate);

   this->ui->treeView->header()->setStretchLastSection(false);
   this->ui->treeView->header()->setVisible(false);
   this->ui->treeView->header()->setSectionResizeMode(0, QHeaderView::Stretch);
   this->ui->treeView->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
   this->ui->treeView->header()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
   this->ui->treeView->header()->setMinimumSectionSize(0);

   this->ui->treeView->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->treeView->setSelectionMode(QAbstractItemView::ExtendedSelection);

   this->ui->treeView->setContextMenuPolicy(Qt::CustomContextMenu);

   this->ui->quickAccessListView->setModel(&this->modelQuickAccess);
   this->ui->quickAccessListView->setSelectionBehavior(QAbstractItemView::SelectRows);

   this->ui->buttonBox->button(QDialogButtonBox::Ok)->setText(tr("Ok"));
   this->ui->buttonBox->button(QDialogButtonBox::Cancel)->setText(tr("Cancel"));

   connect(coreConnection.data(), &RCC::ICoreConnection::disconnected, this, [this]() { this->reject(); });
   connect(
      this->ui->quickAccessListView->selectionModel(),
      &QItemSelectionModel::currentRowChanged,
      this,
      &RemoteBrowseDialog::quickAccessSelectionChanged
   );
   connect(
      this->ui->quickAccessListView,
      &QAbstractItemView::clicked,
      this,
      [this](const QModelIndex& index) {
         // otherwise currentRowChanged handles it
         if (index == this->ui->quickAccessListView->currentIndex())
            this->quickAccessClicked(index);
      }
   );
   connect(&this->model, &RemoteBrowseModel::indexFromPath, this, &RemoteBrowseDialog::selectIndex);
   connect(this->ui->treeView->selectionModel(), &QItemSelectionModel::currentRowChanged,
      this, &RemoteBrowseDialog::treeSelectionChanged);
   connect(this->ui->treeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, [this]() {
      this->ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(
         this->pathValid && this->ui->treeView->selectionModel()->hasSelection());
   });
   connect(this->ui->treeView, &QTreeView::clicked, this, &RemoteBrowseDialog::treeSelectionChanged);
   connect(this->ui->txtPath, &QLineEdit::textEdited, this, &RemoteBrowseDialog::pathEdited);
   connect(this->ui->butPrevious, &QPushButton::clicked, this, [this]() { this->navigateHistory(-1); });
   connect(this->ui->butNext, &QPushButton::clicked, this, [this]() { this->navigateHistory(1); });
   connect(this->ui->butRefresh, &QPushButton::clicked, this, &RemoteBrowseDialog::refresh);
   connect(&this->model, &RemoteBrowseModel::refreshingChanged, this, [this](bool refreshing) {
      this->ui->butRefresh->setEnabled(!refreshing);
   });
   connect(&this->model, &QAbstractItemModel::rowsRemoved, this, [this]() { this->updateNavigation(); });
   connect(this->ui->butUp, &QPushButton::clicked, this, [this]() {
      auto folder = this->ui->treeView->currentIndex();
      if (!this->model.isDirectory(folder))
         folder = folder.parent();
      this->model.cancelPathLookup();
      this->ui->txtPath->setText(this->model.getPath(folder.parent()));
      this->selectIndex(folder.parent());
   });
   const auto selectDefault = [this]() {
      if (!this->ui->quickAccessListView->currentIndex().isValid() && this->modelQuickAccess.rowCount() > 0)
         this->ui->quickAccessListView->setCurrentIndex(this->modelQuickAccess.index(0, 0));
   };
   connect(&this->modelQuickAccess, &QAbstractItemModel::rowsInserted, this, selectDefault);

   connect(
      this->ui->treeView,
      &QTreeView::customContextMenuRequested,
      this,
      &RemoteBrowseDialog::displayContextMenuDownload
   );

   this->setModes(FILE | DIR | SELECT_MULTIPLE);
   this->setPathValid(false);
   this->ui->txtPath->setStyleSheet(QString());
   this->updateNavigation();
   selectDefault();
}

RemoteBrowseDialog::~RemoteBrowseDialog()
{
   delete this->ui;
}

void RemoteBrowseDialog::setModes(Modes modes)
{
   if (modes.testAnyFlag(SELECT_MULTIPLE))
      this->ui->treeView->setSelectionMode(QAbstractItemView::ExtendedSelection);
   else
      this->ui->treeView->setSelectionMode(QAbstractItemView::SingleSelection);

   RemoteBrowseModel::Filters filters;
   if (modes.testAnyFlag(FILE))
      filters |= RemoteBrowseModel::FILE;
   if (modes.testAnyFlag(DIR))
      filters |= RemoteBrowseModel::DIR;
   this->model.setFilters(filters);
}

QStringList RemoteBrowseDialog::getSelectedPaths() const
{
   QStringList result;
   for (const auto& index : this->ui->treeView->selectionModel()->selectedRows())
      result << this->model.getPath(index);
   return result;

}

void RemoteBrowseDialog::accept()
{
   if (this->pathValid && this->ui->treeView->selectionModel()->hasSelection())
      QDialog::accept();
}

void RemoteBrowseDialog::reject()
{
   QDialog::reject();
}

void RemoteBrowseDialog::quickAccessSelectionChanged(const QModelIndex &current, const QModelIndex &previous)
{
   this->quickAccessClicked(current);
}

void RemoteBrowseDialog::quickAccessClicked(const QModelIndex &index)
{
   if (index.isValid())
   {
      const auto path = this->modelQuickAccess.getPath(index);
      if (!path.isEmpty())
      {
         this->ui->txtPath->setText(path);
         this->pathEdited(path);
      }
   }
}

void RemoteBrowseDialog::selectIndex(const QModelIndex &index)
{
   if (!index.isValid())
   {
      this->setPathValid(false);
      return;
   }

   this->selectingPath = true;
   for (QModelIndex parent = index.parent(); parent.isValid(); parent = parent.parent())
      this->ui->treeView->expand(parent);

   this->ui->treeView->expand(index);

   this->ui->treeView->selectionModel()->setCurrentIndex(index,
      QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows);
   this->ui->treeView->scrollTo(index, QAbstractItemView::PositionAtCenter);
   this->selectingPath = false;
   this->visit(index);
}

void RemoteBrowseDialog::treeSelectionChanged(const QModelIndex& index)
{
   if (this->selectingPath)
      return;
   this->model.cancelPathLookup();
   if (!index.isValid())
   {
      this->setPathValid(false);
      this->updateNavigation();
      return;
   }
   this->ui->txtPath->setText(this->model.getPath(index));
   this->visit(index);
}

void RemoteBrowseDialog::pathEdited(const QString& path)
{
   this->setPathValid(false);
   this->model.getIndexFromPath(path);
}

void RemoteBrowseDialog::refresh()
{
   QModelIndexList folders;
   QModelIndexList parents {QModelIndex()};
   while (!parents.isEmpty())
   {
      const auto parent = parents.takeFirst();
      for (int row = 0; row < this->model.rowCount(parent); ++row)
      {
         const auto index = this->model.index(row, 0, parent);
         if (this->model.isDirectory(index) && this->ui->treeView->isExpanded(index))
         {
            folders.append(index);
            parents.append(index);
         }
      }
   }
   // An empty selected folder has no expansion arrow but still needs refreshing.
   auto currentFolder = this->ui->treeView->currentIndex();
   if (!this->model.isDirectory(currentFolder))
      currentFolder = currentFolder.parent();
   if (currentFolder.isValid() && !folders.contains(currentFolder))
      folders.append(currentFolder);
   this->model.refresh(folders);
}

void RemoteBrowseDialog::visit(const QModelIndex& index)
{
   this->setPathValid(true);
   if (!this->navigatingHistory && this->model.isDirectory(index) &&
       (this->historyPosition < 0 || this->history[this->historyPosition] != index))
   {
      this->history.resize(this->historyPosition + 1);
      this->history.append(index);
      this->historyPosition = this->history.size() - 1;
   }
   this->updateNavigation();
}

void RemoteBrowseDialog::navigateHistory(int offset)
{
   const int position = this->historyPosition + offset;
   if (position < 0 || position >= this->history.size())
      return;
   this->model.cancelPathLookup();
   this->historyPosition = position;
   this->navigatingHistory = true;
   this->ui->txtPath->setText(this->model.getPath(this->history[position]));
   this->selectIndex(this->history[position]);
   this->navigatingHistory = false;
}

void RemoteBrowseDialog::updateNavigation()
{
   for (int i = this->history.size() - 1; i >= 0; --i)
   {
      if (!this->history[i].isValid())
      {
         this->history.removeAt(i);
         if (i <= this->historyPosition)
            --this->historyPosition;
      }
   }
   this->ui->butPrevious->setEnabled(this->historyPosition > 0);
   this->ui->butNext->setEnabled(this->historyPosition + 1 < this->history.size());
   auto folder = this->ui->treeView->currentIndex();
   if (!this->model.isDirectory(folder))
      folder = folder.parent();
   this->ui->butUp->setEnabled(folder.parent().isValid());
}

void RemoteBrowseDialog::setPathValid(bool valid)
{
   this->pathValid = valid;
   this->ui->txtPath->setStyleSheet(valid ? QString() : QStringLiteral("QLineEdit { border: 1px solid red; }"));
   this->ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(
      valid && this->ui->treeView->selectionModel()->hasSelection());
}

void RemoteBrowseDialog::displayContextMenuDownload(const QPoint& point)
{
   QPoint globalPosition = this->ui->treeView->mapToGlobal(point);

   if (this->model.isLocal())
   {
      QMenu menu;
      menu.addAction(
         QIcon(":/icons/resources/explore_folder.svg"), tr("Open location"), this, &RemoteBrowseDialog::openLocation
      );
      menu.exec(globalPosition);
   }
}

void RemoteBrowseDialog::openLocation()
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();

   QSet<QString> locations;
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
      locations.insert(this->model.getPath(i.next(), true));

   Utils::openLocations(locations.values(), this);
}
