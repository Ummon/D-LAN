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
  
#include <priv/LocalBrowseResult.h>
using namespace RCC;

#include <QRandomGenerator64>

#include <priv/InternalCoreConnection.h>

LocalBrowseResult::LocalBrowseResult(
   InternalCoreConnection* coreConnection,
   const QString& path,
   bool onlyDirectories,
   int socketTimeout
) :
   ILocalBrowseResult(socketTimeout), coreConnection(coreConnection)
{
   this->browseMessage.set_path(path.toStdString());
   this->browseMessage.set_onlydirectories(onlyDirectories);
   this->browseMessage.set_tag(QRandomGenerator64::global()->generate64());
   connect(coreConnection, &InternalCoreConnection::disconnected, this, [this] {
      this->coreConnection.clear();
      this->waitingForResult = false;
   });
   connect(coreConnection, &InternalCoreConnection::localBrowseResult, this, &LocalBrowseResult::browseResult);
}

void LocalBrowseResult::start()
{
   if (this->started)
      return;
   this->started = true;
   this->startTimer();
   if (!this->coreConnection || !this->coreConnection->isConnected())
      return;

   this->waitingForResult = true;
   this->coreConnection->send(Common::MessageHeader::GUI_LOCAL_BROWSE, this->browseMessage);
}

void LocalBrowseResult::browseResult(const Protos::GUI::LocalBrowseResult& browseResult)
{
   if (this->waitingForResult && !this->isTimedout() && browseResult.tag() == this->browseMessage.tag()) // Is this message for us?
   {
      this->waitingForResult = false;
      this->stopTimer();
      emit result(browseResult.entries());
   }
}
