#include <priv/ChatMessage.h>
using namespace CS;

ChatMessage::ChatMessage(const QString& message, const Common::Hash& ownerID) :
   ID(mtrand.randInt64()), message(message), ownerID(ownerID), time(QDateTime::currentDateTimeUtc())
{
}

QString ChatMessage::getMessage() const
{
   return this->message;
}

MTRand ChatMessage::mtrand;

