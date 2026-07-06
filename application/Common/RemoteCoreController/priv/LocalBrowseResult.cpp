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

#include <priv/Log.h>
#include <priv/InternalCoreConnection.h>

LocalBrowseResult::LocalBrowseResult(InternalCoreConnection* coreConnection, const QString& path, int socketTimeout) :
   ILocalBrowseResult(socketTimeout), coreConnection(coreConnection)
{
   this->browseMessage.set_path(path.toStdString());
   connect(this->coreConnection, &InternalCoreConnection::localBrowseResult, this, &LocalBrowseResult::browseResult);
}

void LocalBrowseResult::start()
{
   this->coreConnection->send(Common::MessageHeader::GUI_LOCAL_BROWSE, this->browseMessage);
   this->startTimer();
}

void LocalBrowseResult::browseResult(const Protos::GUI::LocalBrowseResult& browseResult)
{
   this->stopTimer();
   emit result(browseResult.entries());
}
