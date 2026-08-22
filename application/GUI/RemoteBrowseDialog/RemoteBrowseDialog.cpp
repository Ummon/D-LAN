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
#include <ui_RemoteBrowseDialog.h>
using namespace GUI;

#include <Common/Global.h>
#include <Common/Settings.h>

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

   this->ui->butNext->hide();
   this->ui->butPrevious->hide();
   this->ui->butUp->hide();
   this->ui->txtPath->hide();

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

   this->setModes(FILE | DIR | SELECT_MULTIPLE);
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
         this->model.getIndexFromPath(path);
      }
   }
}

void RemoteBrowseDialog::selectIndex(const QModelIndex &index)
{
   for (QModelIndex parent = index.parent(); parent.isValid(); parent = parent.parent())
      this->ui->treeView->expand(parent);

   this->ui->treeView->expand(index);

   this->ui->treeView->setCurrentIndex(index);
   this->ui->treeView->scrollTo(index, QAbstractItemView::PositionAtCenter);
}