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
#include <QActionGroup>
#include <QInputDialog>
#include <QClipboard>

#include <Common/Global.h>
#include <Common/Settings.h>

#include <Utils.h>

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
   this->ui->tblPeers->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
   this->ui->tblPeers->verticalHeader()->setDefaultSectionSize(QFontMetrics(QApplication::font()).height() + 4);
   this->ui->tblPeers->verticalHeader()->setVisible(false);
   this->ui->tblPeers->setSelectionBehavior(QAbstractItemView::SelectRows);
   this->ui->tblPeers->setSelectionMode(QAbstractItemView::ExtendedSelection);
   this->ui->tblPeers->setShowGrid(false);
   this->ui->tblPeers->setAlternatingRowColors(false);
   this->ui->tblPeers->setContextMenuPolicy(Qt::CustomContextMenu);
   connect(this->ui->tblPeers, &QTableView::customContextMenuRequested, this, &PeersDock::displayContextMenuPeers);
   connect(this->ui->tblPeers, &QTableView::doubleClicked, this, &PeersDock::browse);

   connect(this->coreConnection.data(), &RCC::ICoreConnection::connected, this, &PeersDock::coreConnected);
   connect(this->coreConnection.data(), &RCC::ICoreConnection::disconnected, this, &PeersDock::coreDisconnected);

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
   const QHostAddress addr = i.isValid() ? this->peerListModel.getPeerIP(i.row()) : QHostAddress();

   Protos::GUI::State::Peer::PeerStatus peerStatus = this->peerListModel.getStatus(i.row());

   QMenu menu;
   if (peerStatus == Protos::GUI::State::Peer::OK)
      menu.addAction(QIcon(":/icons/resources/folder.svg"), tr("Browse"), this, &PeersDock::browse);

   if (!addr.isNull())
   {
      if (peerStatus == Protos::GUI::State::Peer::OK)
         menu.addAction(
            QIcon(":/icons/resources/connect.svg"),
            tr("Take control"),
            this,
            [this, addr] { this->takeControlOfACore(addr); }
         );

      menu.addAction(tr("Copy IP: %1").arg(addr.toString()), this, [addr] { QApplication::clipboard()->setText(addr.toString()); });
   }

   menu.addSeparator();

   QAction* sortBySharingAmountAction =
      menu.addAction(
         tr("Sort by the amount of sharing"), this, [this] { this->sortPeers(Protos::GUI::Settings::BY_SHARING_AMOUNT); }
      );

   QAction* sortByNickAction =
      menu.addAction(tr("Sort alphabetically"), this, [this] { this->sortPeers(Protos::GUI::Settings::BY_NICK); });

   QActionGroup sortGroup(&menu);
   sortGroup.setExclusive(true);
   sortBySharingAmountAction->setCheckable(true);
   sortBySharingAmountAction->setChecked(this->peerListModel.getSortType() == Protos::GUI::Settings::BY_SHARING_AMOUNT);
   sortByNickAction->setCheckable(true);
   sortByNickAction->setChecked(this->peerListModel.getSortType() == Protos::GUI::Settings::BY_NICK);
   sortGroup.addAction(sortBySharingAmountAction);
   sortGroup.addAction(sortByNickAction);

   menu.addSeparator();

   menu.addAction(
      QIcon(":/icons/resources/marble_red.svg"),
      tr("Colorize in red"),
      this,
      [this] { this->colorizeSelectedPeer(PeerListModel::COLOR_PEER_RED); }
   );

   menu.addAction(
      QIcon(":/icons/resources/marble_blue.svg"),
      tr("Colorize in blue"),
      this,
      [this] { this->colorizeSelectedPeer(PeerListModel::COLOR_PEER_BLUE); }
   );

   menu.addAction(
      QIcon(":/icons/resources/marble_green.svg"),
      tr("Colorize in green"),
      this,
      [this] { this->colorizeSelectedPeer(PeerListModel::COLOR_PEER_GREEN); }
   );

   menu.addAction(tr("Uncolorize"), this, &PeersDock::uncolorizeSelectedPeer);

   menu.exec(this->ui->tblPeers->mapToGlobal(point));
}

void PeersDock::browse()
{
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedRows())
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

void PeersDock::takeControlOfACore(const QHostAddress& address)
{
   const auto connectToCore = [this, address](const QString& password)
   {
      this->coreConnection->connectToCore(address.toString(), SETTINGS.get<quint32>("core_port"), password);
   };

   // A password is only asked for a remote core.
   if (Common::Global::isLocal(address))
   {
      connectToCore(QString());
      return;
   }

   QInputDialog* inputDialog = new QInputDialog(this);
   inputDialog->setWindowTitle(
      tr("Take control of %1").arg(Common::Global::formatIP(address, SETTINGS.get<quint32>("core_port")))
   );
   inputDialog->setLabelText(tr("Enter a password"));
   inputDialog->setTextEchoMode(QLineEdit::Password);
   inputDialog->resize(300, 100);
   connect(inputDialog, &QInputDialog::textValueSelected, this, [connectToCore](const QString& password)
   {
      if (!password.isEmpty())
         connectToCore(password);
   });
   Utils::showModal(inputDialog);
}

void PeersDock::sortPeers(Protos::GUI::Settings::PeerSortType sortType)
{
   this->peerListModel.setSortType(sortType);
   SETTINGS.set("peer_sort_type", static_cast<quint32>(sortType));
   SETTINGS.save();
}

void PeersDock::colorizeSelectedPeer(const QColor& color)
{
   QSet<Common::Hash> peerIDs;
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedRows())
   {
      this->peerListModel.colorize(i, color);
      peerIDs << this->peerListModel.getPeerID(i.row());
   }

   // Update the settings.
   Protos::GUI::Settings::HighlightedPeers highlightedPeers =
      SETTINGS.get<Protos::GUI::Settings::HighlightedPeers>("highlighted_peers");
   for (int i = 0; i < highlightedPeers.peers_size() && !peerIDs.isEmpty(); i++)
   {
      const Common::Hash peerID(highlightedPeers.peers(i).id().hash());
      if (peerIDs.contains(peerID))
      {
         peerIDs.remove(peerID);
         highlightedPeers.mutable_peers(i)->set_color(color.rgb());
      }
   }

   foreach (Common::Hash peerID, peerIDs)
   {
      Protos::GUI::Settings::HighlightedPeers::Peer* peer = highlightedPeers.add_peers();
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
   foreach (QModelIndex i, this->ui->tblPeers->selectionModel()->selectedRows())
   {
      this->peerListModel.uncolorize(i);
      peerIDs << this->peerListModel.getPeerID(i.row());
   }

   // Update the settings.
   Protos::GUI::Settings::HighlightedPeers highlightedPeers =
      SETTINGS.get<Protos::GUI::Settings::HighlightedPeers>("highlighted_peers");
   for (int i = 0; i < highlightedPeers.peers_size() && !peerIDs.isEmpty(); i++)
   {
      const Common::Hash peerID(highlightedPeers.peers(i).id().hash());
      if (peerIDs.contains(peerID))
      {
         peerIDs.remove(peerID);
         if (i != highlightedPeers.peers_size() - 1)
            highlightedPeers.mutable_peers()->SwapElements(i, highlightedPeers.peers_size() - 1);
         highlightedPeers.mutable_peers()->RemoveLast();
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
   for (int i = 0; i < highlightedPeers.peers_size(); i++)
      this->peerListModel.colorize(highlightedPeers.peers(i).id().hash(), QColor(highlightedPeers.peers(i).color()));
}
