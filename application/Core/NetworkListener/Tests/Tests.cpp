#include <Tests.h>

#include <Builder.h>

#include <QTest>

using namespace NetworkListener;


#include <Common/LogManager/Builder.h>
#include <FileManager/Builder.h>
#include <NetworkListener/Builder.h>
#include <NetworkListener/IChat.h>
#include <PeerManager/Builder.h>


Tests::Tests()
{
}

/**
 * Initialization of the test case, we create all objects needed
 *
 * @author mcuony
 */
void Tests::initTestCase()
{
   this->fileManager = FileManager::Builder::newFileManager();

   this->peerManager = PeerManager::Builder::newPeerManager();

   this->peerManager->setNick("TestCase");

   this->networkListener = NetworkListener::Builder::newNetworkListener(this->peerManager);



}

/**
 * Test if we can send a test message
 *
 * @author mcuony
 */
void Tests::testSending()
{
   NetworkListener::IChat* chat = this->networkListener->getChat();

   //For the next test
   connect(chat, SIGNAL(newMessage(const Protos::Core::ChatMessage&)), this, SLOT(messageRecevied(const Protos::Core::ChatMessage&)));

   if (!chat->send("TEST"))
      QTest::qFail("Unable to send a chat message", "", 0);
}

/**
 * Test if chat message are recevied. For this, we use the previous send message
 *
 * @author mcuony
 */
void Tests::testReception()
{
   //we have to wait for the processing of the event
   QCoreApplication::processEvents(QEventLoop::AllEvents);

   if (!this->isMessageRecevied)
      QTest::qFail("No message recevied...", "", 0);
}

/**
 * Called when a message is recevied
 *
 * @author mcuony
 */
void Tests::messageRecevied(const Protos::Core::ChatMessage& message)
{
   //Do we get a test message ?  (It's important to test as QTest call this function)
   if ( QString::fromStdString(message.message()) == "TEST")
      this->isMessageRecevied = true;
}
