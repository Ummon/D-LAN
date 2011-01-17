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
  
#include <StatusBar.h>
#include <ui_StatusBar.h>
using namespace GUI;

#include <Common/Global.h>
#include <Common/Settings.h>

#include <DialogAbout.h>

StatusBar::StatusBar(QSharedPointer<RCC::ICoreConnection> coreConnection, QWidget *parent)
   : QWidget(parent), ui(new Ui::StatusBar), coreConnection(coreConnection)
{
   this->ui->setupUi(this);
   this->coreDisconnected();

   connect(this->coreConnection.data(), SIGNAL(newState(const Protos::GUI::State&)), this, SLOT(newState(const Protos::GUI::State&)));
   connect(this->coreConnection.data(), SIGNAL(coreConnected()), this, SLOT(coreConnected()));
   connect(this->coreConnection.data(), SIGNAL(coreDisconnected()), this, SLOT(coreDisconnected()));

   connect(this->ui->butHelp, SIGNAL(clicked()), this, SLOT(showAbout()));
}

StatusBar::~StatusBar()
{
   this->coreConnection->disconnect(this); // Disconnect all signals from coreConnection.
   delete this->ui;
}

void StatusBar::coreConnected()
{
   this->updateCoreStatus();
}

void StatusBar::coreDisconnected()
{
   this->updateCoreStatus();
}

void StatusBar::newState(const Protos::GUI::State& state)
{
   this->ui->lblDownloadRate->setText(Common::Global::formatByteSize(state.stats().download_rate()).append("/s"));
   this->ui->lblUploadRate->setText(Common::Global::formatByteSize(state.stats().upload_rate()).append("/s"));

   qint64 totalSharing = 0;
   for (int i = 0; i < state.peer_size(); i++)
      totalSharing += state.peer(i).sharing_amount();
   totalSharing += state.myself().sharing_amount();

   this->ui->lblTotalSharing->setText(Common::Global::formatByteSize(totalSharing));

   this->updateCoreStatus(state.stats().cache_status());
}

void StatusBar::showAbout()
{
   DialogAbout about(this);
   about.exec();
}

void StatusBar::updateCoreStatus(Protos::GUI::State_Stats_CacheStatus status)
{
   QString statusMess("Core: ");

   if (this->coreConnection->isConnected())
   {
      statusMess.append("connected");
      if (!this->coreConnection->isLocal())
         statusMess.append(" to ").append(SETTINGS.get<QString>("core_address"));

      switch (status)
      {
      case Protos::GUI::State_Stats_CacheStatus_SCANNING_IN_PROGRESS:
         statusMess.append(" - scanning in progress..");
         break;
      case Protos::GUI::State_Stats_CacheStatus_HASHING_IN_PROGRESS:
         statusMess.append(" - hashing in progress..");
         break;
      case Protos::GUI::State_Stats_CacheStatus_UP_TO_DATE:
         statusMess.append(" - cache is up to date");
         break;
      case Protos::GUI::State_Stats_CacheStatus_UNKNOWN: // Nothing to display.
      default:;
      }
   }
   else
   {
      statusMess.append("disconnected");
   }

   this->ui->lblCoreStatus->setText(statusMess);
}
