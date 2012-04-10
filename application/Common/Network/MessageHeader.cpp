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
  
#include <Common/Network/MessageHeader.h>
using namespace Common;

#include <Protos/common.pb.h>
#include <Protos/core_protocol.pb.h>
#include <Protos/gui_protocol.pb.h>

#include <ZeroCopyStreamQIODevice.h>

/**
  * @class Common::MessageHeader
  *
  * Contains all type of messages that can be exchange between cores and between gui and core.
  * Can read or write header message.
  * See the *.proto files in "/application/Protos" for more information.
  */

/**
  * Builds a null header, it can be tested with 'isNull()'.
  */
MessageHeader::MessageHeader() :
   type(NULL_MESS), size(0)
{}

MessageHeader::MessageHeader(MessageType type, quint32 size, const Hash senderID) :
   type(type), size(size), senderID(senderID)
{}

const Hash& MessageHeader::getSenderID() const
{
   return this->senderID;
}

quint32 MessageHeader::getSize() const
{
   return this->size;
}

MessageHeader::MessageType MessageHeader::getType() const
{
   return this->type;
}

bool MessageHeader::isNull() const
{
   return this->type == NULL_MESS;
}

void MessageHeader::setNull()
{
   this->type = NULL_MESS;
}

QString MessageHeader::toStr() const
{
   if (this->isNull())
      return QString("MessageHeader : <null>");
   else
      return QString("MessageHeader : type = %1, size = %2, senderID = %3").arg(messToStr(static_cast<MessageType>(this->type))).arg(this->size).arg(this->senderID.toStr());
}

QString MessageHeader::messToStr(MessageType type)
{
   switch (type)
   {
   case NULL_MESS: return "NULL";

   case CORE_IM_ALIVE: return "IM_ALIVE";
   case CORE_CHUNKS_OWNED: return "CHUNKS_OWNED";
   case CORE_CHAT_MESSAGE: return "CHAT_MESSAGE";
   case CORE_FIND: return "FIND";
   case CORE_FIND_RESULT: return "FIND_RESULT";
   case CORE_GET_ENTRIES: return "GET_ENTRIES";
   case CORE_GET_ENTRIES_RESULT: return "GET_ENTRIES_RESULT";
   case CORE_GET_HASHES: return "GET_HASHES";
   case CORE_GET_HASHES_RESULT: return "GET_HASHES_RESULT";
   case CORE_HASH: return "HASH";
   case CORE_GET_CHUNK: return "GET_CHUNK";
   case CORE_GET_CHUNK_RESULT: return "GET_CHUNK_RESULT";

   case GUI_STATE: return "STATE";
   case GUI_STATE_RESULT: return "STATE_RESULT";
   case GUI_EVENT_CHAT_MESSAGES:  return "EVENT_CHAT_MESSAGE";
   case GUI_EVENT_LOG_MESSAGE: return "EVENT_LOG_MESSAGE";
   case GUI_ASK_FOR_AUTHENTICATION: return "ASK_FOR_AUTHENTICATION";
   case GUI_AUTHENTICATION: return "AUTHENTICATION";
   case GUI_AUTHENTICATION_RESULT: return "AUTHENTICATION_RESULT";
   case GUI_LANGUAGE: return "LANGUAGE";
   case GUI_CHANGE_PASSWORD: return "CHANGE_PASSWORD";
   case GUI_SETTINGS: return "SETTINGS";
   case GUI_SEARCH: return "SEARCH";
   case GUI_SEARCH_TAG: return "SEARCH_TAG";
   case GUI_SEARCH_RESULT: return "SEARCH_RESULT";
   case GUI_BROWSE: return "BROWSE";
   case GUI_BROWSE_TAG: return "BROWSE_TAG";
   case GUI_BROWSE_RESULT: return "BROWSE_RESULT";
   case GUI_CANCEL_DOWNLOADS: return "CANCEL_DOWNLOADS";
   case GUI_PAUSE_DOWNLOADS: return "PAUSE_DOWNLOADS";
   case GUI_MOVE_DOWNLOADS: return "MOVE_DOWNLOADS";
   case GUI_DOWNLOAD: return "DOWNLOAD";
   case GUI_CHAT_MESSAGE: return "CHAT_MESSAGE";
   case GUI_REFRESH: return "REFRESH";
   default: return "<UNKNOWN_MESSAGE_TYPE>";
   }
}

/**
  * @exception notEnoughDataException
  */
MessageHeader MessageHeader::readHeader(QIODevice& device, bool skipReadData)
{
   if (device.bytesAvailable() < HEADER_SIZE)
      throw notEnoughDataException();

   char data[HEADER_SIZE];

   if (skipReadData)
      device.read(data, HEADER_SIZE);
   else
      device.peek(data, HEADER_SIZE);

   return MessageHeader::readHeader(data);
}

/**
  * @remarks The buffer size must be at least the header size (28 bytes).
  */
MessageHeader MessageHeader::readHeader(const char* data)
{
   MessageHeader header;

   QByteArray array = QByteArray::fromRawData(data, HEADER_SIZE);
   QDataStream stream(&array, QIODevice::ReadOnly);

   quint32 type;
   stream >> type;
   header.type = static_cast<MessageType>(type);

   stream >> header.size;
   stream >> header.senderID;

   return header;
}

void MessageHeader::writeHeader(QIODevice& device, const MessageHeader& header)
{
   QDataStream stream(&device);
   MessageHeader::writeHeader(stream, header);
}

void MessageHeader::writeHeader(char* buffer, const MessageHeader& header)
{
   // We can't use 'fromRawData' because QByteArray can't modify the given buffer.
   // QByteArray array = QByteArray::fromRawData(buffer, HEADER_SIZE);

   QByteArray array(HEADER_SIZE, 0);
   QDataStream stream(&array, QIODevice::WriteOnly);
   MessageHeader::writeHeader(stream, header);
   memcpy(buffer, array.data(), HEADER_SIZE);
}

inline void MessageHeader::writeHeader(QDataStream& stream, const MessageHeader& header)
{
   stream << header.type;
   stream << header.size;
   stream << header.senderID;
}

#ifdef Q_OS_DARWIN
// For GCC 4.2.
const int MessageHeader::HEADER_SIZE(sizeof(MessageType) + sizeof(quint32) + Hash::HASH_SIZE);
#else
const int MessageHeader::HEADER_SIZE(sizeof(MessageHeader::type) + sizeof(MessageHeader::size) + Hash::HASH_SIZE);
#endif
