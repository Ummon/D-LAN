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
  
#include <CoreConnectionProxy.h>
using namespace Client;

#include <Common/RemoteCoreController/Builder.h>
#include <Common/RemoteCoreController/ISendChatMessageResult.h>

#include <Log.h>

CoreConnectionProxy::CoreConnectionProxy() :
   coreConnection(RCC::Builder::newCoreConnection())
{
   connect(this->coreConnection.data(), &RCC::ICoreConnection::connected, this, &CoreConnectionProxy::connected, Qt::DirectConnection);
}

CoreConnectionProxy::~CoreConnectionProxy()
{
   this->coreConnection->stopLocalCore();
}

void CoreConnectionProxy::setCoreExecutableDirectory(const QString& dir)
{
   this->coreConnection->setCoreExecutableDirectory(dir);
}

void CoreConnectionProxy::connectToCore()
{
   this->coreConnection->connectToCore();
}

void CoreConnectionProxy::connectToCore(int port)
{
   this->coreConnection->connectToCore(port);
}

void CoreConnectionProxy::disconnectFromCore()
{
   this->coreConnection->disconnectFromCore();
}

void CoreConnectionProxy::sendChatMessage(const QString& message)
{
   QSharedPointer<RCC::ISendChatMessageResult> result = this->coreConnection->sendChatMessage(message);
   // We don't care about sending error.
   result->start();
}
