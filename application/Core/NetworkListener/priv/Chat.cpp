#include <priv/Chat.h>
using namespace NL;

#include <Common/ProtoHelper.h>
#include <Common/LogManager/Builder.h>

#include <priv/UDPListener.h>

/**
  * @class Chat
  * @author mcuony
  * @author gburri
  */

Chat::Chat(UDPListener& uDPListener)
   : uDPListener(uDPListener)
{
   // Listening for new messages and forward them to our own signal.
   Chat::connect(
      &this->uDPListener,
      SIGNAL(newChatMessage(const Common::Hash&, const Protos::Core::ChatMessage&)),
      this,
      SIGNAL(newMessage(const Common::Hash&, const Protos::Core::ChatMessage&))
   );
}

void Chat::send(const QString& message)
{
   Protos::Core::ChatMessage chatMessage;
   Common::ProtoHelper::setStr(chatMessage, &Protos::Core::ChatMessage::set_message, message);

   this->uDPListener.send(0x11, chatMessage);
}
