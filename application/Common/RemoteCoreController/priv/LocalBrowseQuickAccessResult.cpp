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
  
#include <priv/LocalBrowseQuickAccessResult.h>
using namespace RCC;

#include <QRandomGenerator64>

#include <priv/InternalCoreConnection.h>

LocalBrowseQuickAccessResult::LocalBrowseQuickAccessResult(InternalCoreConnection* coreConnection, int socketTimeout) :
   ILocalBrowseQuickAccessResult(socketTimeout), coreConnection(coreConnection)
{
   this->browseMessage.set_tag(QRandomGenerator64::global()->generate64());
   connect(coreConnection, &InternalCoreConnection::disconnected, this, [this] {
      this->coreConnection.clear();
      this->waitingForResult = false;
   });
   connect(coreConnection, &InternalCoreConnection::localBrowseQuickAccessResult, this, &LocalBrowseQuickAccessResult::browseResult);
}

void LocalBrowseQuickAccessResult::start()
{
   if (this->started)
      return;
   this->started = true;
   this->startTimer();
   if (!this->coreConnection || !this->coreConnection->isConnected())
      return;

   this->waitingForResult = true;
   this->coreConnection->send(Common::MessageHeader::GUI_LOCAL_BROWSE_QUICK_ACCESS, this->browseMessage);
}

void LocalBrowseQuickAccessResult::browseResult(const Protos::GUI::LocalBrowseQuickAccessResult& browseResult)
{
   if (this->waitingForResult && !this->isTimedout() && browseResult.tag() == this->browseMessage.tag()) // Is this message for us?
   {
      this->waitingForResult = false;
      this->stopTimer();
      emit result(browseResult.quick_access());
   }
}
