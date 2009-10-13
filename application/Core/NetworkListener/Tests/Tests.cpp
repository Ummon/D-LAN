#include <Tests.h>

#include <Builder.h>

using namespace NetworkListener;

Tests::Tests()
{
}

void Tests::initTestCase()
{

}


/*
::Core::Core() : QObject(),  logger(LogManager::Builder::newLogger("Core"))
{
   this->logger->log("Loading ..", LogManager::EndUser);

   this->fileManager = FileManager::Builder::newFileManager();

   this->peerManager = PeerManager::Builder::newPeerManager();

   this->peerManager->setNick("Test");

   this->networkListener = NetworkListener::Builder::newNetworkListener(this->peerManager);

   this->logger->log("Ready to serve", LogManager::EndUser);

   this->peerManager->setNick("Test2");


    // Testing chat function.
   NetworkListener::IChat* chat = this->networkListener->getChat();

   // Listening for chat event.
   connect(chat, SIGNAL(newMessage(const Protos::Core::ChatMessage&)), this, SLOT(dBug_chat(const Protos::Core::ChatMessage&)));

   this->logger->log("Listening for new messages..", LogManager::Debug);

   // Sending a message.
   chat->send("Je suis un canard");



}


void ::Core::dBug_chat(const Protos::Core::ChatMessage& message)
{
    this->logger->log("Got a message ! (" + QString::fromStdString(message.peerid().hash()) + ") " + QString::fromStdString(message.message()), LogManager::EndUser);
}
*/
