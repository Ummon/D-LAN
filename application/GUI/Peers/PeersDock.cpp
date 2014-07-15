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
  
#include <Peers/PeersDock.h>
#include <ui_PeersDock.h>
using namespace GUI;

#include <QHostAddress>
#include <QMenu>
#include <QInputDialog>
#include <QClipboard>

#include <Common/Global.h>
#include <Common/Settings.h>

Q_DECLARE_METATYPE(QHostAddress)

PeersDock::PeersDock(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent) :
   QDockWidget(parent),
   ui(new Ui::PeersDock),
   coreConnection(coreConnection),
   peerListModel(this->coreConnection)
{
   this->ui->setupUi(this);

   this->peerListModel.setSortType(static_cast<Protos::GUI::Settings::PeerSortType>(SETTINGS.get<quint32>("peer_sort_type")));

   this->ui->tblPeers->setModel(&this->peerListModel);
   this->ui->tblPeers->setItemDelegate(&this->peerListDelegate);
   this->ui->tblPeers->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
   this->ui->tblPeers->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
   this->ui->tblPeers->horizontalHeader()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
   this->ui->tblPeers->horizontalHeader()->setVisible(false);
   this->ui->tblPeers->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed); // TODO: is there an another way to reduce the row size?
   this->ui->tblPeers->verticalHeader()->setDefaultSectionSize(QApplication::fontMetrics().height() + 4);
   this->ui->tblPeers->verticalHeader()->setVisible(false);
   this->ui->tblPeers->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblPeers->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->tblPeers->setShowGrid(false);
   this->ui->tblPeers->setAlternatingRowColors(false);
   this->ui->tblPeers->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->tblPeers, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(displayContextMenuPeers(QPoint)));
   connect(this->ui->tblPeers, SIGNAL(doubleClicked(QModelIndex)), this, SLOT(browse()));

   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected(bool)));

   this->restoreColorizedPeers();
   this->coreDisconnected(false); // Initial state.
}

PeersDock::~PeersDock()
{
   delete this->ui;
}

PeerListModel& PeersDock::getModel()
{
   return this->peerListModel;
}

void PeersDock::changeEvent(QEvent* event)
{
   if (event->type() == QEvent::LanguageChange)
      this->ui->retranslateUi(this);

   QDockWidget::changeEvent(event);
}

void PeersDock::displayContextMenuPeers(const QPoint& point)
{
   QModelIndex i = this->ui->tblPeers->currentIndex();
   QHostAddress addr = i.isValid() ? this->peerListModel.getPeerIP(i.row()) : QHostAddress();
   QVariant addrVariant;
   addrVariant.setValue(addr);

   Protos::GUI::State::Peer::PeerStatus peerStatus = this->peerListModel.getStatus(i.row());

   QMenu menu;
   if (peerStatus == Protos::GUI::State::Peer::OK)
      menu.addAction(QIcon(":/icons/ressources/folder.png"), tr("Browse"), this, SLOT(browse()));

   if (!addr.isNull())
   {
      if (peerStatus == Protos::GUI::State::Peer::OK)
      {
         QAction* takeControlAction = menu.addAction(QIcon(":/icons/ressources/lightning.png"), tr("Take control"), this, SLOT(takeControlOfACore()));
         takeControlAction->setData(addrVariant);
      }

      QAction* copyIPAction = menu.addAction(tr("Copy IP: %1").arg(addr.toString()), this, SLOT(copyIPToClipboard()));
      copyIPAction->setData(addrVariant);
   }

   menu.addSeparator();

   QAction* sortBySharingAmountAction = menu.addAction(tr("Sort by the amount of sharing"), this, SLOT(sortPeersBySharingAmount()));
   QAction* sortByNickAction = menu.addAction(tr("Sort alphabetically"), this, SLOT(sortPeersByNick()));

   QActionGroup sortGroup(this);
   sortGroup.setExclusive(true);
   sortBySharingAmountAction->setCheckable(true);
   sortBySharingAmountAction->setChecked(this->peerListModel.getSortType() == Protos::GUI::Settings::BY_SHARING_AMOUNT);
   sortByNickAction->setCheckable(true);
   sortByNickAction->setChecked(this->peerListModel.getSortType() == Protos::GUI::Settings::BY_NICK);
   sortGroup.addAction(sortBySharingAmountAction);
   sortGroup.addAction(sortByNickAction);

   menu.addSeparator();

   menu.addAction(QIcon(":/icons/ressources/marble_red.png"), tr("Colorize in red"), this, SLOT(colorizeSelectedPeer()))->setData(QColor(128, 0, 0));
   menu.addAction(QIcon(":/icons/ressources/marble_blue.png"), tr("Colorize in blue"), this, SLOT(colorizeSelectedPeer()))->setData(QColor(0, 0, 128));
   menu.addAction(QIcon(":/icons/ressources/marble_green.png"), tr("Colorize in green"), this, SLOT(colorizeSelectedPeer()))->setData(QColor(0, 128, 0));
   menu.addAction(tr("Uncolorize"), this, SLOT(uncolorizeSelectedPeer()));

   menu.exec(this->ui->tblPeers->mapToGlobal(point));
}

void PeersDock::browse()
{
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedIndexes())
   {
      if (i.isValid())
      {
         Protos::GUI::State::Peer::PeerStatus peerStatus = this->peerListModel.getStatus(i.row());
         if (peerStatus == Protos::GUI::State::Peer::OK)
         {
            Common::Hash peerID = this->peerListModel.getPeerID(i.row());
            if (!peerID.isNull())
               emit browsePeer(peerID);
         }
      }
   }

   this->ui->tblPeers->clearSelection();
}

void PeersDock::takeControlOfACore()
{
   QAction* action = dynamic_cast<QAction*>(this->sender());
   if (action)
   {
      QHostAddress address = action->data().value<QHostAddress>();
      QString password;

      if (!Common::Global::isLocal(address))
      {
         QInputDialog inputDialog(this);
         inputDialog.setWindowTitle(tr("Take control of %1").arg(Common::Global::formatIP(address, SETTINGS.get<quint32>("core_port"))));
         inputDialog.setLabelText(tr("Enter a password"));
         inputDialog.setTextEchoMode(QLineEdit::Password);
         inputDialog.resize(300, 100);

         if (inputDialog.exec() == QDialog::Rejected || inputDialog.textValue().isEmpty())
            return;

         password = inputDialog.textValue();
      }

      this->coreConnection->connectToCore(address.toString(), SETTINGS.get<quint32>("core_port"), password);
   }
}

void PeersDock::copyIPToClipboard()
{
   QAction* action = dynamic_cast<QAction*>(this->sender());
   if (action)
   {
      QHostAddress address = action->data().value<QHostAddress>();
      QApplication::clipboard()->setText(address.toString());
   }
}

void PeersDock::sortPeersBySharingAmount()
{
   this->peerListModel.setSortType(Protos::GUI::Settings::BY_SHARING_AMOUNT);
   SETTINGS.set("peer_sort_type", static_cast<quint32>(Protos::GUI::Settings::BY_SHARING_AMOUNT));
   SETTINGS.save();
}

void PeersDock::sortPeersByNick()
{
   this->peerListModel.setSortType(Protos::GUI::Settings::BY_NICK);
   SETTINGS.set("peer_sort_type", static_cast<quint32>(Protos::GUI::Settings::BY_NICK));
   SETTINGS.save();
}

/**
  * Must be called only by a 'QAction' object whith a 'QColor' object as data.
  */
void PeersDock::colorizeSelectedPeer()
{
   const QColor color = static_cast<QAction*>(this->sender())->data().value<QColor>();

   QSet<Common::Hash> peerIDs;
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedIndexes())
   {
      this->peerListModel.colorize(i, color);
      peerIDs << this->peerListModel.getPeerID(i.row());
   }

   // Update the settings.
   Protos::GUI::Settings::HighlightedPeers highlightedPeers = SETTINGS.get<Protos::GUI::Settings::HighlightedPeers>("highlighted_peers");
   for (int i = 0; i < highlightedPeers.peer_size() && !peerIDs.isEmpty(); i++)
   {
      const Common::Hash peerID(highlightedPeers.peer(i).id().hash());
      if (peerIDs.contains(peerID))
      {
         peerIDs.remove(peerID);
         highlightedPeers.mutable_peer(i)->set_color(color.rgb());
      }
   }

   foreach (Common::Hash peerID, peerIDs)
   {
      Protos::GUI::Settings::HighlightedPeers::Peer* peer = highlightedPeers.add_peer();
      peer->mutable_id()->set_hash(peerID.getData(), Common::Hash::HASH_SIZE);
      peer->set_color(color.rgb());
   }

   SETTINGS.set("highlighted_peers", highlightedPeers);
   SETTINGS.save();

   this->ui->tblPeers->clearSelection();
}

void PeersDock::uncolorizeSelectedPeer()
{
   QSet<Common::Hash> peerIDs;
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedIndexes())
   {
      this->peerListModel.uncolorize(i);
      peerIDs << this->peerListModel.getPeerID(i.row());
   }

   // Update the settings.
   Protos::GUI::Settings::HighlightedPeers highlightedPeers = SETTINGS.get<Protos::GUI::Settings::HighlightedPeers>("highlighted_peers");
   for (int i = 0; i < highlightedPeers.peer_size() && !peerIDs.isEmpty(); i++)
   {
      const Common::Hash peerID(highlightedPeers.peer(i).id().hash());
      if (peerIDs.contains(peerID))
      {
         peerIDs.remove(peerID);
         if (i != highlightedPeers.peer_size() - 1)
            highlightedPeers.mutable_peer()->SwapElements(i, highlightedPeers.peer_size() - 1);
         highlightedPeers.mutable_peer()->RemoveLast();
         i--;
      }
   }

   SETTINGS.set("highlighted_peers", highlightedPeers);
   SETTINGS.save();

   this->ui->tblPeers->clearSelection();
}

void PeersDock::coreConnected()
{
   this->ui->tblPeers->setEnabled(true);
}

void PeersDock::coreDisconnected(bool force)
{
   this->ui->tblPeers->setEnabled(false);
}

void PeersDock::restoreColorizedPeers()
{
   Protos::GUI::Settings::HighlightedPeers highlightedPeers = SETTINGS.get<Protos::GUI::Settings::HighlightedPeers>("highlighted_peers");
   for (int i = 0; i < highlightedPeers.peer_size(); i++)
      this->peerListModel.colorize(highlightedPeers.peer(i).id().hash(), QColor(highlightedPeers.peer(i).color()));
}
