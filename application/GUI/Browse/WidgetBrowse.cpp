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
#include <QShowEvent>

void BrowseDelegate::paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const
{
   QStyleOptionViewItemV4 newOption(option);
   newOption.state = option.state & (~QStyle::State_HasFocus);
   QStyledItemDelegate::paint(painter, newOption, index);
}

/////

WidgetBrowse::WidgetBrowse(QSharedPointer<RCC::ICoreConnection> coreConnection, const PeerListModel& peerListModel, const DirListModel& sharedDirsModel, const Common::Hash& peerID, QWidget *parent) :
   QWidget(parent),
   ui(new Ui::WidgetBrowse),
   downloadMenu(coreConnection, sharedDirsModel),
   coreConnection(coreConnection),
   peerID(peerID),
   browseModel(coreConnection, sharedDirsModel, peerID, false) // 'false' because the model is automatically refreshed when the widget is shown, see 'WidgetBrowse::showEvent(..)'.
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
   connect(this->ui->treeView, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(displayContextMenuDownload(const QPoint&)));

   if (this->coreConnection->getOurID() == this->peerID)
      this->ui->butDownload->hide();
   else
      connect(this->ui->butDownload, SIGNAL(clicked()), this, SLOT(download()));

   connect(&this->downloadMenu, SIGNAL(downloadTo(const Common::Hash&, const QString&)), this, SLOT(downloadTo(const Common::Hash&, const QString&)));

   this->setWindowTitle(QString("[%1]").arg(peerListModel.getNick(this->peerID)));
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

void WidgetBrowse::displayContextMenuDownload(const QPoint& point)
{
   QPoint globalPosition = this->ui->treeView->mapToGlobal(point);
   if (this->coreConnection->getOurID() == this->peerID)
   {
      if (this->coreConnection->isLocal())
      {
         QMenu menu;
         menu.addAction("Open location", this, SLOT(openLocation()));
         menu.exec(globalPosition);
      }
   }
   else
   {
      this->downloadMenu.show(globalPosition);
   }
}

void WidgetBrowse::download()
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
      this->coreConnection->download(this->peerID, this->browseModel.getEntry(i.next()));
}

void WidgetBrowse::downloadTo(const Common::Hash& sharedDirID, const QString& path)
{
   QModelIndexList selectedRows = this->ui->treeView->selectionModel()->selectedRows();
   for (QListIterator<QModelIndex> i(selectedRows); i.hasNext();)
      this->coreConnection->download(this->peerID, this->browseModel.getEntry(i.next()), sharedDirID, path);
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

void WidgetBrowse::showEvent(QShowEvent* event)
{
   if (!event->spontaneous())
      this->refresh();
}
