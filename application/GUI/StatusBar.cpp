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
  
#include <StatusBar.h>
#include <ui_StatusBar.h>
using namespace GUI;

#include <Common/Global.h>
#include <Common/Settings.h>

#include <DialogAbout.h>

StatusBar::StatusBar(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget* parent) :
   QWidget(parent), ui(new Ui::StatusBar), coreConnection(coreConnection)
{
   this->ui->setupUi(this);
   this->coreDisconnected();

   connect(this->coreConnection.data(), SIGNAL(newState(const Protos::GUI::State&)), this, SLOT(newState(const Protos::GUI::State&)));
   connect(this->coreConnection.data(), SIGNAL(connected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(disconnected(bool)), this, SLOT(coreDisconnected()));

   connect(this->ui->butHelp, SIGNAL(clicked()), this, SLOT(showAbout()));

   connect(this->ui->butLog, SIGNAL(toggled(bool)), this, SIGNAL(showDockLog(bool)));
}

StatusBar::~StatusBar()
{
   this->coreConnection->disconnect(this); // Disconnect all signals from coreConnection.
   delete this->ui;
}

void StatusBar::dockLogVisibilityChanged(bool visibility)
{
   this->ui->butLog->setChecked(visibility);
}

void StatusBar::coreConnected()
{
   this->updateCoreStatus();
}

void StatusBar::coreDisconnected()
{
   this->setDownloadRate(0);
   this->setUploadRate(0);
   this->setTotalSharing(0, 0);
   this->updateCoreStatus();
}

void StatusBar::newState(const Protos::GUI::State& state)
{
   this->setDownloadRate(state.stats().download_rate());
   this->setUploadRate(state.stats().upload_rate());

   qint64 totalSharing = 0;
   for (int i = 0; i < state.peer_size(); i++)
      totalSharing += state.peer(i).sharing_amount();
   totalSharing += state.myself().sharing_amount();
   this->setTotalSharing(state.peer_size() + 1, totalSharing);

   this->updateCoreStatus(state.stats().cache_status(), state.stats().progress());
}

void StatusBar::showAbout()
{
   DialogAbout about(this);
   about.exec();
}

void StatusBar::setDownloadRate(qint64 rate)
{
   this->ui->lblDownloadRate->setText(Common::Global::formatByteSize(rate).append("/s"));
}

void StatusBar::setUploadRate(qint64 rate)
{
   this->ui->lblUploadRate->setText(Common::Global::formatByteSize(rate).append("/s"));
}

void StatusBar::setTotalSharing(int nbPeer, qint64 amount)
{
   this->ui->lblTotalSharing->setText(QString::number(nbPeer).append(" ").append(nbPeer > 1 ? tr("peers") : tr("peer")).append(": ").append(Common::Global::formatByteSize(amount)));
}

void StatusBar::updateCoreStatus(Protos::GUI::State_Stats_CacheStatus status, int progress)
{
   QString statusMess("Core: ");

   if (this->coreConnection->isConnected())
   {
      if (!this->coreConnection->isLocal())
         statusMess.append(QString(tr("connected to %1").arg(SETTINGS.get<QString>("core_address"))));
      else
         statusMess.append(tr("connected"));

      switch (status)
      {
      case Protos::GUI::State_Stats_CacheStatus_LOADING_CACHE_IN_PROGRESS:
         statusMess.append(" - ").append(tr("loading cache.."));
         this->ui->prgCurrentAction->setVisible(true);
         this->ui->prgCurrentAction->setValue(progress);
         break;
      case Protos::GUI::State_Stats_CacheStatus_SCANNING_IN_PROGRESS:
         statusMess.append(" - ").append(tr("scanning in progress.."));
         this->ui->prgCurrentAction->setVisible(false);
         break;
      case Protos::GUI::State_Stats_CacheStatus_HASHING_IN_PROGRESS:
         statusMess.append(" - ").append(tr("hashing in progress.."));
         this->ui->prgCurrentAction->setVisible(true);
         this->ui->prgCurrentAction->setValue(progress);
         break;
      case Protos::GUI::State_Stats_CacheStatus_UP_TO_DATE:
         statusMess.append(" - ").append(tr("cache is up to date"));
         this->ui->prgCurrentAction->setVisible(false);
         this->ui->prgCurrentAction->setValue(progress);
         break;
      case Protos::GUI::State_Stats_CacheStatus_UNKNOWN: // Nothing to display.
      default:;
         this->ui->prgCurrentAction->setVisible(false);
      }
   }
   else
   {
      statusMess.append(tr("disconnected"));
      this->ui->prgCurrentAction->setVisible(false);
   }

   this->ui->lblCoreStatus->setText(statusMess);
}
