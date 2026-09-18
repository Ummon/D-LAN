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
#include <google/protobuf/io/coded_stream.h>

/**
  * The 'Message' object take the ownership of the given protobuf message.
  */
Message::Message(const MessageHeader& header, google::protobuf::Message* message) :
   header(header), message(message)
{
}

bool Message::isNull() const
{
   return this->message.isNull();
}

const MessageHeader& Message::getHeader() const
{
   return this->header;
}

int Message::writeMessageToBuffer(
   char* buffer,
   quint32 bufferSize,
   const MessageHeader& header,
   const google::protobuf::Message* message
)
{
   if (MessageHeader::HEADER_SIZE + header.getSize() > bufferSize)
      return 0;

   MessageHeader::writeHeader(buffer, header);

   if (message && !message->SerializeToArray(buffer + MessageHeader::HEADER_SIZE, bufferSize - MessageHeader::HEADER_SIZE))
       return 0;

   return MessageHeader::HEADER_SIZE + header.getSize();
}

int Message::writeMessageToDevice(QIODevice* ioDevice, const MessageHeader& header, const google::protobuf::Message* message)
{
   if (!MessageHeader::writeHeader(*ioDevice, header))
      return 0;

   if (message)
   {
      ZeroCopyOutputStreamQIODevice outputStream(ioDevice);
      if (!message->SerializeToZeroCopyStream(&outputStream) || !outputStream.Flush())
         return 0;
   }

   return MessageHeader::HEADER_SIZE + header.getSize();
}

int Message::writeMessageToDeviceWithCachedSizes(QIODevice* ioDevice, const MessageHeader& header, const google::protobuf::Message* message)
{
   // Cached-size serialization bypasses protobuf's usual initialization check.
   if (message && !message->IsInitialized())
      return 0;

   if (!MessageHeader::writeHeader(*ioDevice, header))
      return 0;

   if (message)
   {
      ZeroCopyOutputStreamQIODevice outputStream(ioDevice);
      {
         google::protobuf::io::CodedOutputStream codedStream(&outputStream);
         message->SerializeWithCachedSizes(&codedStream);
         // Return unused bytes before flushing the QIODevice adapter. Otherwise
         // the final buffer could include bytes beyond the serialized message.
         codedStream.Trim();
         if (codedStream.HadError() || codedStream.ByteCount() != header.getSize())
            return 0;
      }
      if (!outputStream.Flush())
         return 0;
   }

   return MessageHeader::HEADER_SIZE + header.getSize();
}

/**
  * @exception ReadErrorException
  */
Message Message::readMessage(const char* buffer, quint32 bufferSize)
{
   // The header is read from the buffer, it must entirely be there. Tested first to avoid
   // an unsigned underflow in the test of the body size below.
   if (bufferSize < static_cast<quint32>(MessageHeader::HEADER_SIZE))
      throw ReadErrorException();

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
