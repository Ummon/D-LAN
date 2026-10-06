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

#include <Browse/BrowseWidget.h>
#include <ui_BrowseWidget.h>
using namespace GUI;

#include <QMenu>
#include <QPainter>
#include <QIcon>
#include <QUrl>
#include <QShowEvent>

#include <Common/ProtoHelper.h>

#include <Log.h>

BrowseWidget::BrowseWidget(
   QSharedPointer<RCC::ICoreConnection> coreConnection,
   const PeerListModel& peerListModel,
   const SharedEntryListModel& sharedEntryListModel,
   const Common::Hash& peerID,
   QWidget* parent
) :
   EntriesWidget(coreConnection, parent),
   ui(new Ui::BrowseWidget),
   downloadMenu(coreConnection, sharedEntryListModel),
   peerID(peerID),
   browseModel(coreConnection, sharedEntryListModel, peerID),
   tryingToReachEntryToBrowse(false)
{
   this->ui->setupUi(this);
   this->setEntries(&this->browseModel, this->ui->treeView);

   this->ui->treeView->setModel(&this->browseModel);
   this->ui->treeView->setItemDelegate(&this->browseDelegate);
   this->ui->treeView->header()->setVisible(false);
   this->ui->treeView->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->treeView->header()->setSectionResizeMode(1, QHeaderView::Stretch);

   this->ui->treeView->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->treeView->setSelectionMode(QAbstractItemView::ExtendedSelection);

   this->ui->treeView->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->treeView, &QTreeView::customContextMenuRequested, this, &BrowseWidget::displayContextMenuDownload);
   connect(this->ui->treeView, &QTreeView::doubleClicked, this, &BrowseWidget::entryDoubleClicked);

   if (this->coreConnection->getRemoteID() == this->peerID)
      this->ui->butDownload->hide();
   else
      connect(this->ui->butDownload, &QPushButton::clicked, this, &BrowseWidget::download);

   connect(&this->downloadMenu, &DownloadMenu::download, this, &BrowseWidget::download);
   connect(&this->downloadMenu, qOverload<>(&DownloadMenu::downloadTo), this, qOverload<>(&BrowseWidget::downloadTo));
   connect(
      &this->downloadMenu,
      qOverload<const Common::Hash&, const Common::Path&>(&DownloadMenu::downloadTo),
      this,
      qOverload<const Common::Hash&, const Common::Path&>(&BrowseWidget::downloadTo)
   );

   connect(&this->browseModel, &BrowseModel::loadingResultFinished, this, &BrowseWidget::tryToReachEntryToBrowse);

   this->setWindowTitle(peerListModel.getNick(this->peerID));
}

BrowseWidget::~BrowseWidget()
{
    delete this->ui;
}

Common::Hash BrowseWidget::getPeerID() const
{
   return this->peerID;
}

void BrowseWidget::browseTo(const Protos::Common::Entry& remoteEntry)
{
   this->tryingToReachEntryToBrowse = true;
   this->remoteEntryToBrowse = remoteEntry;

   if (!this->browseModel.isWaitingResult())
      this->tryToReachEntryToBrowse();
}

void BrowseWidget::refresh()
{
   this->browseModel.refresh();
}

void BrowseWidget::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);

   EntriesWidget::changeEvent(event);
}

/**
  * All the entries are owned by the browsed peer.
  */
Common::Hash BrowseWidget::entryPeerID(const QModelIndex&) const
{
   return this->peerID;
}

void BrowseWidget::displayContextMenuDownload(const QPoint& point)
{
   QPoint globalPosition = this->ui->treeView->mapToGlobal(point);
   if (this->coreConnection->getRemoteID() == this->peerID)
   {
      if (this->coreConnection->isLocal())
      {
         QMenu menu;
         menu.addAction(
            QIcon(":/icons/resources/explore_folder.svg"), tr("Open location"), this, &BrowseWidget::openLocation
         );
         menu.exec(globalPosition);
      }
   }
   else
   {
      this->downloadMenu.show(globalPosition);
   }
}

void BrowseWidget::entryDoubleClicked(const QModelIndex& index)
{
   this->openFile(index);
}

/**
  * Try to select an entry from a remote peer in the browse tab.
  * The entry to browse is set in 'this->remoteEntryToBrowse'.
  */
void BrowseWidget::tryToReachEntryToBrowse()
{
   if (!this->tryingToReachEntryToBrowse)
      return;

   // First we search for the shared directory of the entry.
   for (int r = 0; r < this->browseModel.rowCount(); r++)
   {
      QModelIndex currentIndex = this->browseModel.index(r, 0);
      const Protos::Common::Entry& root = this->browseModel.getEntry(currentIndex);
      if (
         root.has_shared_entry() &&
         this->remoteEntryToBrowse.has_shared_entry() &&
         root.shared_entry().id().hash() == this->remoteEntryToBrowse.shared_entry().id().hash()
      )
      {
         // Then we try to match each folder name. If a folder cannot be reached and the content of the last folder
         // isn't loaded yet then we ask to expand this last folder.
         // After the folder entries are loaded, 'tryToReachEntryToBrowse()' will be recalled
         // via the signal 'BrowseModel::loadingResultFinished()'.
         const QStringList& path =
            QString::fromStdString(this->remoteEntryToBrowse.path())
               .append(QString::fromStdString(this->remoteEntryToBrowse.name()))
               .split('/', Qt::SkipEmptyParts);

         for (QStringListIterator i(path); i.hasNext();)
         {
            QModelIndex childIndex = this->browseModel.searchChild(i.next(), currentIndex);
            if (!childIndex.isValid())
            {
               if (this->browseModel.hasUnloadedChildren(currentIndex))
               {
                  this->ui->treeView->expand(currentIndex);
                  return;
               }

               // The content of the folder is known and the entry isn't in it: it doesn't exist anymore, we give up.
               // Otherwise this folder would be expanded again each time some entries are loaded.
               break;
            }
            currentIndex = childIndex;

            // We reach the last entry name (file or directory), we just have to show and select it.
            if (!i.hasNext())
            {
               this->ui->treeView->scrollTo(currentIndex);
               this->ui->treeView->selectionModel()->select(
                  currentIndex,
                  QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows
               );
            }
         }
      }
   }

   this->tryingToReachEntryToBrowse = false;
}

