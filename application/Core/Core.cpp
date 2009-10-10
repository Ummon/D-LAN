#include <Core.h>
using namespace Core;

#include <Common/LogManager/Builder.h>
#include <FileManager/Builder.h>
#include <NetworkListener/Builder.h>
#include <NetworkListener/IChat.h>
 #include <QObject>

::Core::Core()
      : QObject(),  logger(LogManager::Builder::newLogger("Core"))
{
   this->logger->log("Loading ..", LogManager::EndUser);

   this->fileManager = FileManager::Builder::newFileManager();

   this->networkListener = NetworkListener::Builder::newNetworkListener();

   this->logger->log("Ready to serve", LogManager::EndUser);


    /*///////////////////////
      DEBUGGING CODE, HAS TO BE REMOVED OR MOVED INTO TESTING UNITS
    ////////////////////////*/

    //Testing chat function
   NetworkListener::IChat* chat = this->networkListener->getChat();

   //Listening for chat event
   connect(chat, SIGNAL(newMessage(const Protos::Core::ChatMessage&)), this, SLOT(dBug_chat(const Protos::Core::ChatMessage&)));

   this->logger->log("Listening for new messages..", LogManager::Debug);

   //Sending a message
   chat->send("Je suis un canard");

    /*///////////////////////
      END OF DEBUGGING CODE
    ////////////////////////*/

}

/*///////////////////////
  DEBUGGING FUNCTION, HAS TO BE REMOVED OR MOVED INTO TESTING UNITS
////////////////////////*/
void ::Core::dBug_chat(const Protos::Core::ChatMessage& message) {

    QString texte;

    this->logger->log("Got a message ! " + texte.fromStdString(message.message()), LogManager::EndUser);
}
/*///////////////////////
  END OF DEBUGGING FUNCTION
////////////////////////*/
