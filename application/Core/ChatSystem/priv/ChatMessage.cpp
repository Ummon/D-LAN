#include <priv/ChatMessage.h>
using namespace CS;

#include <Common/ProtoHelper.h>

ChatMessage::ChatMessage(const QString& message, const Common::Hash& ownerID, const QString& ownerNick) :
   ID(mtrand.randInt64()),
   message(message),
   ownerID(ownerID),
   time(QDateTime::currentDateTimeUtc()),
   ownerNick(ownerNick)
{
}

ChatMessage::ChatMessage(const Protos::Common::ChatMessage& chatMessage) :
   ID(chatMessage.id()),
   message(Common::ProtoHelper::getStr(chatMessage, &Protos::Common::ChatMessage::message)),
   ownerID(chatMessage.has_peer_id() ? chatMessage.peer_id().hash() : Common::Hash()),
   time(chatMessage.has_time() ? QDateTime::fromMSecsSinceEpoch(chatMessage.time()) : QDateTime::currentDateTimeUtc()),
   ownerNick(chatMessage.has_peer_nick() ? Common::ProtoHelper::getStr(chatMessage, &Protos::Common::ChatMessage::peer_nick) : QString())
{

}

quint64 ChatMessage::getID() const
{
   return this->ID;
}

QDateTime ChatMessage::getTime() const
{
   return this->time;
}

void ChatMessage::fillProtoChatMessage(Protos::Common::ChatMessage& protoChatMessage) const
{
   protoChatMessage.set_id(this->ID);
   Common::ProtoHelper::setStr(protoChatMessage, &Protos::Common::ChatMessage::set_message, this->message);
   protoChatMessage.set_time(this->time.toMSecsSinceEpoch());
   protoChatMessage.mutable_peer_id()->set_hash(this->ownerID.getData(), Common::Hash::HASH_SIZE);
   Common::ProtoHelper::setStr(protoChatMessage, &Protos::Common::ChatMessage::set_peer_nick, this->ownerNick);
}

MTRand ChatMessage::mtrand;

