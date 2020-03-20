#pragma once

#include <tuple>

#include <QString>

#include <FlatBuffersSchema/common_fbs.h>
using namespace FBS::Common;
#include <FlatBuffersSchema/core_protocol_fbs.h>
#include <FlatBuffersSchema/gui_protocol_fbs.h>

#include <Common/Network/MessageHeader.h>

namespace Common
{
   class MessageFBS
   {
   public:
      MessageFBS();

      static std::tuple<QString, quint64, quint32> readOldIMAliveMessage(const char* buffer);

      template <typename T>
      static MessageFBS readMessageBody(const MessageHeader& header, const char* buffer);
   };
}
