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
  
#include <Common/Network/Message.h>
using namespace Common;

#include <QIODevice>

Message::Message(const MessageHeader& header, google::protobuf::Message* message) :
   header(header), message(message)
{
}

//Message::Message(const MessageHeader::MessageType type, const QSharedPointer<google::protobuf::Message>& message) :
//   header(type), message(message)
//{
//}

///**
//  * The object takes the control of 'message' and will destroy it when no more used.
//  */
//Message::Message(const MessageHeader::MessageType type, google::protobuf::Message* message) :
//   header(type), message(message)
//{
//}

bool Message::isNull() const
{
   return this->message.isNull();
}

const MessageHeader& Message::getHeader() const
{
   return this->header;
}

//void Message::setHeaderSize(quint32 size)
//{
//   this->header.setSize(size);
//}

//void Message::setHeaderPeerID(const Hash& ID)
//{
//   this->header.setSenderID(ID);
//}

int Message::writeMessageToBuffer(char* buffer, quint32 bufferSize, const MessageHeader& header, const google::protobuf::Message* message)
{
   if (MessageHeader::HEADER_SIZE + header.getSize() > bufferSize)
      return 0;

   MessageHeader::writeHeader(buffer, header);

   if (message)
      message->SerializeToArray(buffer + MessageHeader::HEADER_SIZE, bufferSize - MessageHeader::HEADER_SIZE);

   return MessageHeader::HEADER_SIZE + header.getSize();
}

int Message::writeMessageToDevice(QIODevice* ioDevice, const MessageHeader& header, const google::protobuf::Message* message)
{
   MessageHeader::writeHeader(*ioDevice, header);

   if (message)
   {
      ZeroCopyOutputStreamQIODevice outputStream(ioDevice);
      if (!message->SerializeToZeroCopyStream(&outputStream))
         return 0;
   }

   return MessageHeader::HEADER_SIZE + header.getSize();
}

/**
  * @exception ReadErrorException
  */
Message Message::readMessage(const char* buffer, quint32 bufferSize)
{
   MessageHeader header = MessageHeader::readHeader(buffer);

   if (header.getSize() > bufferSize - MessageHeader::HEADER_SIZE)
      throw ReadErrorException();

   const char* bufferBody = buffer + MessageHeader::HEADER_SIZE;

   return readMessageBody(header, bufferBody);
}

Message Message::readMessageBodyFromDevice(const MessageHeader& header, QIODevice* ioDevice)
{
   ZeroCopyInputStreamQIODevice inputStream(ioDevice);
   return readMessageBody(header, &inputStream);
}
