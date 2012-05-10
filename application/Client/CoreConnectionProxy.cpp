#include <CoreConnectionProxy.h>
using namespace Client;

#include <Common/RemoteCoreController/Builder.h>

CoreConnectionProxy::CoreConnectionProxy() :
   coreConnection(RCC::Builder::newCoreConnection())
{
   connect(this->coreConnection.data(), SIGNAL(connected()), this, SIGNAL(connected()), Qt::DirectConnection);
}

void CoreConnectionProxy::tryConnecting()
{
   this->coreConnection->connectToCore();
}

void CoreConnectionProxy::sendChatMessage(const QString& message)
{
   this->coreConnection->sendChatMessage(message);
}
