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
  
#include <Browse/WidgetBrowse.h>
#include <ui_WidgetBrowse.h>
using namespace GUI;

#include <QMenu>
#include <QPainter>
#include <QIcon>
#include <QDesktopServices>
#include <QUrl>

void BrowseDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);
   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

WidgetBrowse::WidgetBrowse(CoreConnection& coreConnection, PeerListModel& peerListModel, const Common::Hash& peerID, QWidget *parent)
   : QWidget(parent), ui(new Ui::WidgetBrowse), coreConnection(coreConnection), peerListModel(peerListModel), peerID(peerID), browseModel(coreConnection, peerID)
{
   this->ui->setupUi(this);

   this->ui->treeView->setModel(&this->browseModel);
   this->ui->treeView->setItemDelegate(&this->browseDelegate);
   this->ui->treeView->header()->setVisible(false);
   this->ui->treeView->header()->setResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->treeView->header()->setResizeMode(1, QHeaderView::Stretch);

   this->ui->treeView->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->treeView->setSelectionMode(QAbstractItemView::ExtendedSelection);

   this->ui->treeView->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->treeView, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuPeers(const QPoint&)));

   connect(this->ui->butDownload, SIGNAL(clicked()), this, SLOT(download()));

   this->setWindowTitle(QString("[%1]").arg(this->peerListModel.getNick(this->peerID)));
}

WidgetBrowse::~WidgetBrowse()
{
    delete this->ui;
}

Common::Hash WidgetBrowse::getPeerID() const
{
   return this->peerID;
}

void WidgetBrowse::refresh()
{
   this->browseModel.refresh();
}

void WidgetBrowse::displayContextMenuPeers(const QPoint& point)
{
   if (this->coreConnection.getOurID() == this->peerID)
   {
      if (this->coreConnection.isLocal())
      {
         QMenu menu;
         menu.addAction("Open location", this, SLOT(openLocation()));
         menu.exec(this->ui->treeView->mapToGlobal(point));
      }
   }
   else
   {
      QMenu menu;
      menu.addAction(QIcon(":/icons/ressources/download.png"), "Download selected entries", this, SLOT(download()));
      menu.exec(this->ui->treeView->mapToGlobal(point));
   }
}

void WidgetBrowse::download()
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      this->coreConnection.download(this->peerID, this->browseModel.getEntry(i.next()));
   }
}

void WidgetBrowse::openLocation()
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();

   QSet<QString> locations;
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
   {
      QModelIndex index = i.next();
      locations.insert("file:///" + this->browseModel.getLocationPath(index));
   }

   for (QSetIterator<QString> i(locations); i.hasNext();)
      QDesktopServices::openUrl(QUrl(i.next(), QUrl::TolerantMode));
}
