#include "MessageFBS.h"

#include <Common/Network/MessageFBS.h>
using namespace Common;

#include <QString>

#include <Common/ProtoHelper.h>

/**
  * @class Common::MessageFBS
  *
  * New message type using FlatBuffers replacing the old 'Message' type.
  */

MessageFBS::MessageFBS()
{
}

/**
  * @return Tuple: (nick, amount shared, protocol version).
  */
std::tuple<QString, quint64, quint32> MessageFBS::readOldIMAliveMessage(const char* buffer)
{
   const quint8* p = reinterpret_cast<const quint8*>(buffer);

   quint32 protocolVersion = ProtoHelper::readUInt<quint32>(p);
   /*quint32 port =*/ ProtoHelper::readUInt<quint32>(p); // Port ignored.
   QString nick = ProtoHelper::readString(p);
   quint64 amount = ProtoHelper::readUInt<quint64>(p);

   return { nick, amount, protocolVersion };
}
